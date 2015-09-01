; docformat = 'rst'

;+
; Scattering within the comp birefringent filter causes stray light to be
; superimposed on the CoMP images. This is evident by a distribution of light
; present in areas that should be dark, behind the occulting disk and outside
; the comp field-of-view. This routine fits the stray light outside the CoMP
; field-of-view with a polynomial and uses the fit to interpolate the stray
; light over the comp field-of-view, which is subrtacted from the input image.
;
; :Examples:
;   For example, call like::
;
;     comp_fix_stray_light, image, occulter1, occulter2, field1, field2, fit
;
; :Uses:
;   comp_sfit, comp_eval_surf
;
; :Returns:
;   image is returned with the stray light fit subtracted from it.
;
; :Params:
;   image : in, out, required, type="fltarr(1024, 1024)"
;     the 1024x1024 CoMP image to be stray light corrected
;   flat_header : in, required, type=strarr
;     header from a flat image containing occulter and field stop positions
;   fit : out, optional, type="fltarr(1024, 1024)"
;     the fit which was subtracted from image
;
; :Author:
;   Tomczyk, modified by Sitongia
;
; :History:
;   10/8/14 - polywarp was replaced by sfit to increase speed
;-
pro comp_fix_stray_light, image, flat_header, fit
  compile_opt strictarr

  s = size(image)   ; get image size
  nx = s[1]

  ; create arrays of x coordinate and y coordinate assuming image is square

  x = rebin(findgen(nx), nx, nx) - float(nx) / 2.
  y = transpose(x)

  ; create mask with cutoff inside of occulter and outside of field mask

  image_mask = comp_annulus_1024(flat_header, o_offset=-20.0, f_offset=+28.0)

  ; identify pixels to use. Omit pixels in dark corners of flat image.
  ; the version of this line which does not omit the corners of the image is:
  ; good=where(image_mask eq 0.0,count)

  good = where(image_mask eq 0.0 and y lt (- x + 600.) and y gt (- x - 600.), $
               count)

  ; fit stray light outside of field-of-view

  data = fltarr(3, count, /nozero)
  data[0, *] = x[good]
  data[1, *] = y[good]
  data[2, *] = image[good]
  ndeg = 2    ; degree of polynomial fit
  kx = comp_sfit(data, ndeg)

  ; compute fit from coefficients and subtract it from input image
  fit = comp_eval_surf(transpose(kx), reform(x[*, 0]), reform(y[0, *]))
  ; fit = fltarr(nx, nx)
  ; for i = 0, ndeg do begin
  ;   for j = 0, ndeg do begin
  ;     fit += kx[j, i] * x^i * y^j
  ;   endfor
  ; endfor

  image -= fit
end
