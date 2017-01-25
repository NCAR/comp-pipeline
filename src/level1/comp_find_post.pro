; docformat = 'rst'

;+
; Procedure to find the position angle of the occulter post in comp data. This
; routine interpolates the comp annulus to r-theta coordinates and averages
; over `r` to find the intensity variation with theta which is then fit with a
; gaussian to determine the location of the occulter post.
;
; :Uses:
;   comp_config_common, comp_constants_common
;
; :Params:
;   image : in
;     the image in which to find the post angle
;   occulter : in
;     structure containing the parameters of the occulting disk {x,y,radius} (pixels)
;   field : in
;     structure containing the parameters of the field stop {x,y,radius} (pixels)
;   pa : out
;     the position angle of the post (degrees) measured counter-clockwise from
;     the top of the image
;
; :Author:
;   sitongia, modified by Tomczyk
;
; :History:
;   added gaussian fit and comments - 10/24/14 ST
;   replaced average with IDL built-in mean 01/07/15 GdT 
;-
pro comp_find_post, image, occulter, field, pa
  compile_opt idl2
  @comp_constants_common
  @comp_config_common

  ans = ' '
  debug = 0   ; debug mode just for this routine (1=yes, 0=no)

  new_nx = 1500     ; sampling in theta direction
  new_ny = 40       ; sampling in radius direction

  ; this is theta from 0 to 2*pi
  new_th = rebin(findgen(new_nx) / float(new_nx) * (2. * !dpi), new_nx, new_ny)

  ; this is r from the occulter radius to the field stop radius
  new_r = rebin(transpose((field.r - occulter.r) * findgen(new_ny) / float(new_ny - 1) $
                            + occulter.r), $
                new_nx, new_ny)

  ; now convert to rectangular coordinates
  new_x = new_r * cos(new_th) + nx * 0.5 + occulter.x
  new_y = new_r * sin(new_th) + ny * 0.5 + occulter.y

  ; use bilinear to extract the values
  new_img = bilinear(image, new_x ,new_y)

  ; extract center of annulus to avoid overlap and off-center
  ; bottom trim 10 above occulter edge, top trim 20 below field stop
  new_img = new_img[*, 10:new_ny - 20]

  ; average over y
  theta_scan = mean(new_img, dimension=2)

  ; fit the inverted intensity with a gaussian, use the location of maximum as
  ; a guess for the post position
  y = median(theta_scan) - theta_scan
  x = findgen(new_nx) / float(new_nx) * 360.
  max_value = max(y, max_pixel)

  yfit = gaussfit(x, y, coeff, $
                  nterms=5, $
                  estimates=[max(y), x[max_pixel], 15., 0., 0.])

  mg_log, 'coeff: %s', strjoin(string(coeff, format='(%"%0.3f")'), ', '), $
          name='comp/find_post', /debug

  if (debug eq 1) then begin
    plot, x, y, xtitle='Angle (degrees)', ytitle='Intensity', title='find_post'
    oplot, x, yfit
    read, 'enter return', ans
  endif

  ; Rotate into coordinate system that solar position angles are referenced,
  ; namely from the top of the image (north) instead of the mathematical polar
  ; coordinate system, where theta=0 is on the right. This angle is measured
  ; CCW from North.
  pa = coeff[1] - 90.0
end
