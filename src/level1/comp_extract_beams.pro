;+
; comp_extact_beams:
;
; This subroutine extracts the two beams from a CoMP dual-beam
; (raw or close to raw) image set.
;
; :Uses:
;   comp_inventory_header, comp_extract_time, comp_extract1, comp_extract2, sun
;
; :Params:
;   images : in, required, type="fltarr(nx, ny, nimg)"
;     the (array) of images which contain both beams on the same image plane
;   headers : in, required, type="strarr(ntags, nimg)"
;     the headers corresponding to the images
;   date_dir : in, required, type=string
;     the directory for containing the files for the date in question, used to
;     find the flat file.
;   d1 : out, required, type="fltarr(620, 620, nimg)"
;     the images from the upper left beam, sized 620x620
;   d2 : out, required, type="fltarr(620, 620, nimg)"
;     the images from the lower right beam
;
; :Keywords:
;   image_geometry : in, required, type=structure
;     image geometry specifications
;
; :Author:
;   Joseph Plowman
;-
pro comp_extract_beams, images, headers, date_dir, d1, d2, $
                        image_geometry=image_geometry
  compile_opt strictarr
  @comp_constants_common
  @comp_config_common

  comp_inventory_header, headers, beam, wave, pol, type, expose, cover, $
                         cal_pol, cal_ret
  time = comp_extract_time(headers, day, month, year, hours, mins, secs)

  ; compute solar ephemeris quantities from date and time (add 10 hours to
  ; convert from Hawaii time to UTC)
  sun, year, month, day, 10.0 + hours + mins / 60. + secs / 3600., $
       pa=p_angle, sd=semi_diam, true_ra=sol_ra, true_dec=sol_dec, lat0=b0

  ; retrieve distortion coefficients in file: dx1_c, dy1_c, dx2_x, dy2_c
  restore, filename=filepath(distortion_coeffs_file, root=binary_dir)

  ; set up matrix for image rotation
  x0 = float(nx) / 2.0
  y0 = float(ny) / 2.0

  x = rebin(findgen(nx) - x0, nx, nx)
  y = transpose(rebin(findgen(nx) - y0, nx, nx))

  angle = p_angle + 180.0   ; raw image oriented south up
  xp = x * cos(angle * !dtor) - y * sin(angle * !dtor)
  yp = x * sin(angle * !dtor) + y * cos(angle * !dtor)

  ; compute image offsets
  xpp1 = xp + x0 + image_geometry.occulter1.x
  ypp1 = yp + y0 + image_geometry.occulter1.y
  xpp2 = xp + x0 + image_geometry.occulter2.x
  ypp2 = yp + y0 + image_geometry.occulter2.y

  nimg = n_elements(images[0, 0, *])
  d1 = fltarr(nx, nx, nimg)
  d2 = fltarr(nx, nx, nimg)
  for i = 0L, nimg - 1L do begin
    ; extract sub-arrays
    sub1 = comp_extract1(images[*, *, i])
    sub2 = comp_extract2(images[*, *, i])

    ; remove distortion
    comp_apply_distortion, sub1, sub2, dx1_c, dy1_c, dx2_c, dy2_c

    ; translate and rotate images
    d1[*, *, i] = interpolate(sub1, xpp1, ypp1, missing=0.0, cubic=-0.5)
    d2[*, *, i] = interpolate(sub2, xpp2, ypp2, missing=0.0, cubic=-0.5)
  endfor
end
