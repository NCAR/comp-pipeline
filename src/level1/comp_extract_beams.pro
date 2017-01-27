;+
; This subroutine extracts the two beams from a CoMP dual-beam
; (raw or close to raw) image set.
;
; :Uses:
;   comp_inventory_header, comp_extract_time, comp_extract1, comp_extract2, sun
;
; :Params:
;   images : in, required, type="fltarr(1024, 1024, nimg)"
;     the array of images which contain both beams on the same image plane
;   headers : in, required, type="strarr(ntags, nimg)"
;     the headers corresponding to the images
;   date_dir : in, required, type=string
;     the directory for containing the files for the date in question, used to
;     find the flat file
;   d1 : out, required, type="fltarr(620, 620, nimg)"
;     the images from the upper left beam
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

  ; determine if UL beam if off the detector
  off_left = image_geometry.field1.r gt (image_geometry.field1.x + nx / 2)
  off_top = (image_geometry.field1.r + image_geometry.field1.y + 1024 - ny / 2) gt 1023
  off1 = off_left || off_top

  ; determine if the LR beam if off the detector
  off_right = (image_geometry.field2.r + image_geometry.field2.x + 1024 - nx / 2) gt 1023
  off_bottom = image_geometry.field2.r lt (image_geometry.field2.y + ny / 2)
  off2 = off_right || off_bottom

  if (off1) then begin
    annulus_mask1 = comp_disk_mask(image_geometry.occulter1.r, $
                                   dx=image_geometry.occulter1.x, $
                                   dy=image_geometry.occulter1.y) $
                      and comp_field_mask(image_geometry.field1.r, $
                                          dx=image_geometry.field1.x, $
                                          dy=image_geometry.field1.y)
  endif
  if (off2) then begin
    annulus_mask2 = comp_disk_mask(image_geometry.occulter2.r, $
                                   dx=image_geometry.occulter2.x, $
                                   dy=image_geometry.occulter2.y) $
                      and comp_field_mask(image_geometry.field2.r, $
                                          dx=image_geometry.field2.x, $
                                          dy=image_geometry.field2.y)
  endif

  n_images = n_elements(images[0, 0, *])
  d1 = fltarr(nx, nx, n_images)
  d2 = fltarr(nx, nx, n_images)
  for i = 0L, n_images - 1L do begin
    ; extract sub-arrays
    sub1 = comp_extract1(images[*, *, i])
    sub2 = comp_extract2(images[*, *, i])

    ; remove distortion
    comp_apply_distortion, sub1, sub2, dx1_c, dy1_c, dx2_c, dy2_c

    ; if background is in UL and it is off detector, then use mean of UL annulus
    if (beam[i] gt 0.0 && off1) then begin
      missing1 = total(sub1 * annulus_mask1) / total(annulus_mask1)
    endif else missing1 = 0.0

    ; if background is in LR and it is off detector, then use mean of LR annulus
    if (beam[i] lt 0.0 && off2) then begin
      missing2 = total(sub2 * annulus_mask2) / total(annulus_mask2)
    endif else missing2 = 0.0

    ; translate and rotate images
    d1[*, *, i] = interpolate(sub1, xpp1, ypp1, missing=missing1, cubic=-0.5)
    d2[*, *, i] = interpolate(sub2, xpp2, ypp2, missing=missing2, cubic=-0.5)
  endfor
end
