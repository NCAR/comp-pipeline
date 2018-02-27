;+
; This subroutine extracts the two beams from a CoMP dual-beam
; (raw or close to raw) image set.
;
; :Uses:
;   comp_inventory_header, comp_extract_time, comp_extract1, comp_extract2, sun,
;   comp_apply_distortion
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
;     image geometry specifications from distortion corrected images
;   uncorrected_geometry : in, required, type=structure
;     image geometry specifications from distortion uncorrected images
;
; :Author:
;   MLSO Software Team
;-
pro comp_extract_beams, images, headers, date_dir, d1, d2, $
                        image_geometry=image_geometry, $
                        uncorrected_geometry=uncorrected_geometry
  compile_opt strictarr
  @comp_constants_common
  @comp_config_common
  @comp_check_common

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

  x = findgen(nx,ny)mod(nx) -  nx*0.5 + 0.5
  y = transpose(findgen(ny,nx)mod(ny) ) - ny*0.5 + 0.5
 
  angle = p_angle + 180.0   ; raw image oriented south up
  xp = x * cos(angle * !dtor) - y * sin(angle * !dtor)
  yp = x * sin(angle * !dtor) + y * cos(angle * !dtor)

  ; correct center to account for image offset
  xpp1 = xp + image_geometry.occulter1.x
  ypp1 = yp + image_geometry.occulter1.y
  xpp2 = xp + image_geometry.occulter2.x
  ypp2 = yp + image_geometry.occulter2.y

  ; determine if UL beam if off the detector
  left_edge = (uncorrected_geometry.field1.x + nx / 2) - uncorrected_geometry.field1.r
  top_edge = 1023 - (uncorrected_geometry.field1.r + uncorrected_geometry.field1.y + 1024 - ny / 2)

  mg_log, 'left edge gap = %0.1f pixels', left_edge, name='comp', /debug
  mg_log, 'top edge gap = %0.1f pixels', top_edge, name='comp', /debug

  off_left = uncorrected_geometry.field1.r gt (uncorrected_geometry.field1.x + nx / 2)
  off_top = (uncorrected_geometry.field1.r + uncorrected_geometry.field1.y + 1024 - ny / 2) gt 1023
  off1 = off_left || off_top

  ; determine if the LR beam if off the detector
  right_edge = 1023 - (uncorrected_geometry.field2.r + uncorrected_geometry.field2.x + 1024 - nx / 2)
  bottom_edge = (uncorrected_geometry.field2.y + ny / 2) - uncorrected_geometry.field2.r

  mg_log, 'right edge gap = %0.1f pixels', right_edge, name='comp', /debug
  mg_log, 'bottom edge gap = %0.1f pixels', bottom_edge, name='comp', /debug

  off_right = (uncorrected_geometry.field2.r + uncorrected_geometry.field2.x + 1024 - nx / 2) gt 1023
  off_bottom = uncorrected_geometry.field2.r gt (uncorrected_geometry.field2.y + ny / 2)
  off2 = off_right || off_bottom

  if (off1) then begin
    annulus_mask1 = comp_disk_mask(uncorrected_geometry.occulter1.r, $
                                   xcen=uncorrected_geometry.occulter1.x, $
                                   ycen=uncorrected_geometry.occulter1.y) $
                      and comp_field_mask(uncorrected_geometry.field1.r, $
                                          xcen=uncorrected_geometry.field1.x, $
                                          ycen=uncorrected_geometry.field1.y)
    n_images_off_detector += 1
  endif
  if (off2) then begin
    annulus_mask2 = comp_disk_mask(uncorrected_geometry.occulter2.r, $
                                   xcen=uncorrected_geometry.occulter2.x, $
                                   ycen=uncorrected_geometry.occulter2.y) $
                      and comp_field_mask(uncorrected_geometry.field2.r, $
                                          xcen=uncorrected_geometry.field2.x, $
                                          ycen=uncorrected_geometry.field2.y)
    n_images_off_detector += 1
  endif

  k1 = 0.99353
  k2 = 1.00973

  n_images = n_elements(images[0, 0, *])
  d1 = fltarr(nx, ny, n_images)
  d2 = fltarr(nx, ny, n_images)
  for i = 0L, n_images - 1L do begin
    ; extract sub-arrays
    sub1 = comp_extract1(images[*, *, i])
    sub2 = comp_extract2(images[*, *, i])

    ; remove distortion
    sub1 = comp_apply_distortion(sub1, k1)
    sub2 = comp_apply_distortion(sub2, k2)

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
