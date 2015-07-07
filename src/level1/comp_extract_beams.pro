;+
; comp_extact_beams:
;
; This subroutine extracts the two beams from a CoMP dual-beam
; (raw or close to raw) image set.
;
; :Uses:
;   comp_inventory_header, comp_extract_time, comp_image_geometry,
;   comp_extract1, comp_extract2
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
; :Author:
;   Joseph Plowman
;-
pro comp_extract_beams, images, headers, date_dir, d1, d2
  compile_opt strictarr

  comp_inventory_header, headers, beam, group, wave, pol, type, expose, cover, $
                         cal_pol, cal_ret
  time = comp_extract_time(headers, day, month, year, hours, mins, secs)

  ; TODO: should we use SUN here?

  ; compute solar ephemeris quantities from date and time
  jd = tojd(day, month, year, hours, mins, secs) + 10.0 / 24.0
  ephem2, jd, sol_ra, sol_dec, b0, p_angle, semi_diam, sid_time, dist, $
          xsun, ysun, zsun

  nx = 620
  ny = nx

  ; compute transformation arrays for distortion removal
  x = rebin(findgen(nx), nx, nx)
  y = transpose(x)

  k1 = 0.99353
  x1new = x * 0.5 * (1.0 + k1) + y * 0.5 * (1.0 - k1)
  y1new = x * 0.5 * (1.0 - k1) + y * 0.5 * (1.0 + k1)
  k2 = 1.00973
  x2new = x * 0.5 * (1.0 + k2) + y * 0.5 * (1.0 - k2)
  y2new = x * 0.5 * (1.0 - k2) + y * 0.5 * (1.0 + k2)

  comp_image_geometry, headers, date_dir, $
                       occulter1, occulter2, $
                       field1, field2, $
                       post_angle1, post_angle2, $
                       delta_x, delta_y, $
                       overlap_angle

  ; set up matrix for image rotation
  x0 = float(nx) / 2.0
  y0 = float(ny) / 2.0

  x = rebin(findgen(nx) - x0, nx, nx)
  y = transpose(rebin(findgen(nx) - y0, nx, nx))

  angle = p_angle + 180.0   ; don't know why Tomczyk added 180 here
  xp = x * cos(angle * !pi / 180.0) - y * sin(angle * !pi / 180.0)
  yp = x * sin(angle * !pi / 180.0) + y * cos(angle * !pi / 180.0)

  ; compute image offsets
  xpp1 = xp + x0 + occulter1.x
  ypp1 = yp + y0 + occulter1.y
  xpp2 = xp + x0 + occulter2.x
  ypp2 = yp + y0 + occulter2.y

  nimg = n_elements(images[0, 0, *])
  d1 = fltarr(nx, nx, nimg)
  d2 = fltarr(nx, nx, nimg)
  for i = 0L, nimg - 1L do begin
    ; extract sub-arrays
    d1[*, *, i] = comp_extract1(images[*, *, i])
    d2[*, *, i] = comp_extract2(images[*, *, i])

    ; remove distortion
    d1[*, *, i] = interpolate(d1[*, *, i], x1new, y1new, cubic=-0.5, missing=0.0)
    d2[*, *, i] = interpolate(d2[*, *, i], x2new, y2new, cubic=-0.5, missing=0.0)

    ; translate and rotate images
    d1[*, *, i] = interpolate(d1[*, *, i], xpp1, ypp1, missing=0.0, cubic=-0.5)
    d2[*, *, i] = interpolate(d2[*, *, i], xpp2, ypp2, missing=0.0, cubic=-0.5)
  endfor
end