; docformat = 'rst'

;+
; Extract various image geometry parameters from the appropriate flat file.
;
; :Uses:
;   comp_inventory_header, comp_extract_time, comp_read_flats, sxpar,
;   comp_apply_distortion
;
; :Returns:
;   structure
;
; :Params:
;   images : in, required, type="fltarr(620, 620, nimg)"
;     images to use to find field stop/occulter centers
;   headers : in, required, type="strarr(ntags, nimg)"
;     the FITS headers from which we want the geometry
;   date_dir : in, required, type=string
;     the directory for containing the files for the date in question, used to
;     find the flat file
;
; :Keywords:
;   primary_header : in, required, type=strarr
;     primary FITS header
;   uncorrected_geometry : out, optional, type=structure
;     set to a named variable to retrieve a structure with fields `occulter1`,
;     `occulter2`, `field1`, and `field2` -- all which are also structures with
;     fields `x`, `y`, and `r` -- as well as fields `post_angle1` and
;     `post_angle2`
;
; :Author:
;   MLSO Software Team
;-
function comp_image_geometry, images, headers, date_dir, $
                              primary_header=primary_header, $
                              uncorrected_geometry=uncorrected_geometry
  @comp_constants_common
  @comp_config_common
  @comp_diagnostics_common

  ; scan the headers to find out what observations the files contain
  comp_inventory_header, headers, beam, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret
  mg_log, 'beams: %s', strjoin(strtrim(beam, 2), ','), name='comp', /debug

  wave_type = comp_find_wavelength(wave, /name)

  ; get the time in the format preferred by read_flats
  time = comp_extract_time(headers, day, month, year, hours, mins, secs)
  comp_read_flats, date_dir, wave, beam, time, flat, flat_header, flat_waves, $
                   flat_names, flat_expose

  ; retrieve distortion coefficients in file: dx1_c, dy1_c, dx2_x, dy2_c
  restore, filename=filepath(distortion_coeffs_file, root=binary_dir)
 
  ;read occulter center in 620x620 frames to use as center guess
  occulter1 = {x:sxpar(flat_header, 'OXCNTER1') - 1.0, $
               y:sxpar(flat_header, 'OYCNTER1') - 1.0 - 1024 + ny, $
               r:sxpar(flat_header, 'ORADIUS1')}
  occulter2 = {x:sxpar(flat_header, 'OXCNTER2') - 1.0 - 1024 + nx, $
               y:sxpar(flat_header, 'OYCNTER2') - 1.0, $
               r:sxpar(flat_header, 'ORADIUS2')}

  ; field position
  field1 = {x:sxpar(flat_header, 'FXCNTER1') - 1.0, $
            y:sxpar(flat_header, 'FYCNTER1') - 1.0 - 1024 + ny, $
            r:sxpar(flat_header, 'FRADIUS1')}
  field2 = {x:sxpar(flat_header, 'FXCNTER2') - 1.0 - 1024 + nx, $
            y:sxpar(flat_header, 'FYCNTER2') - 1.0, $
            r:sxpar(flat_header, 'FRADIUS2')}

  if (arg_present(uncorrected_geometry)) then begin
    uncorrected_geometry = {occulter1: {x:sxpar(flat_header, 'OXCNTRU1') - 1.0, $
                                        y:sxpar(flat_header, 'OYCNTRU1') - 1.0 - 1024 + ny, $
                                        r:sxpar(flat_header, 'ORADU1')}, $
                            occulter2: {x:sxpar(flat_header, 'OXCNTRU2') - 1.0 - 1024 + nx, $
                                        y:sxpar(flat_header, 'OYCNTRU2') - 1.0, $
                                        r:sxpar(flat_header, 'ORADU2')}, $
                            field1: {x:sxpar(flat_header, 'FXCNTRU1') - 1.0, $
                                     y:sxpar(flat_header, 'FYCNTRU1') - 1.0 - 1024 + ny, $
                                     r:sxpar(flat_header, 'FRADU1')}, $
                            field2: {x:sxpar(flat_header, 'FXCNTRU2') - 1.0 - 1024 + nx, $
                                     y:sxpar(flat_header, 'FYCNTRU2') - 1.0, $
                                     r:sxpar(flat_header, 'FRADU2')}, $
                            post_angle1: sxpar(flat_header, 'PSTANGU1'), $
                            post_angle2: sxpar(flat_header, 'PSTANGU2') $
                           }
  endif

  ; beam -1: corona in UL (comp_extract1), beam 1: corona in LR (comp_extract2)

  ind1 = where(beam gt 0, n_plus_beam)
  if (n_plus_beam gt 0) then begin
    sub1 = comp_extract1(reform(images[*, *, ind1[0]]))

    if (arg_present(uncorrected_geometry)) then begin
      comp_find_annulus, sub1, uncorrected_calc_occulter1, $
                         occulter_guess=[uncorrected_geometry.occulter1.x, $
                                         uncorrected_geometry.occulter1.y, $
                                         uncorrected_geometry.occulter1.r], $
                         occulter_points=occulter_points1, $
                         /occulter_only, $
                         error=error
      if (error ne 0L) then begin
        mg_log, 'error finding center in uncorrected image', name='comp', /warn
        ; TODO: skip this image
      endif

      uncorrected_geometry.occulter1 = uncorrected_calc_occulter1
    endif

    ; remove distortion
    sub1 = comp_apply_distortion(sub1, dx1_c, dy1_c)

    comp_find_annulus, sub1, calc_occulter1, $
                       occulter_guess=[occulter1.x, $
                                       occulter1.y, $
                                       occulter1.r], $
                       /occulter_only, $
                       error=error
    if (error ne 0L) then begin
      mg_log, 'error finding center', name='comp', /warn
      ; TODO: skip this image
    endif

    ; the output of comp_find_annulus is an offset from the original guess, so
    ; so we must add the offsets together to get the final offset
    ;calc_occulter1.x += occulter1.x
    ;calc_occulter1.y += occulter1.y
  
    mg_log, '%s, %f, %f, %f, %f, %d', $
            wave_type, $
            time, $
            calc_occulter1.x, $
            calc_occulter1.y, $
            calc_occulter1.r, ind1[0], $
            name='image.occ.ul', /debug
  endif

  ind2 = where(beam lt 0, n_minus_beam)
  if (n_minus_beam gt 0) then begin
    sub2 = comp_extract2(reform(images[*, *, ind2[0]]))

    if (arg_present(uncorrected_geometry)) then begin
      comp_find_annulus, sub2, uncorrected_calc_occulter2, $
                         occulter_guess=[uncorrected_geometry.occulter2.x, $
                                         uncorrected_geometry.occulter2.y, $
                                         uncorrected_geometry.occulter2.r], $
                         /occulter_only, $
                         error=error
      if (error ne 0L) then begin
        mg_log, 'error finding center in uncorrected image', name='comp', /warn
        ; TODO: skip this image
      endif

      uncorrected_geometry.occulter2 = uncorrected_calc_occulter2
    endif

    ; remove distortion
    sub2 = comp_apply_distortion(sub2, dx2_c, dy2_c)

    comp_find_annulus, sub2, calc_occulter2, $
                       occulter_guess=[occulter2.x, $
                                       occulter2.y, $
                                       occulter2.r], $
                       occulter_points=occulter_points2, $
                       /occulter_only, $
                       error=error
    if (error ne 0L) then begin
      mg_log, 'error finding center', name='comp', /warn
      ; TODO: skip this image
    endif

    ; the output of comp_find_annulus is an offset from the original guess, so
    ; so we must add the offsets together to get the final offset
    ;calc_occulter2.x += occulter2.x
    ;calc_occulter2.y += occulter2.y

    mg_log, '%s, %f, %f, %f, %f, %d', $
            wave_type, $
            time, $
            calc_occulter2.x, $
            calc_occulter2.y, $
            calc_occulter2.r, $
            ind2[0], $
            name='image.occ.lr', /debug
  endif

  ; write flat centers

  mg_log, '%s, %f, %f, %f, %f', $
          wave_type, time, $
          occulter1.x, occulter1.y, occulter1.r, $
          name='flat.occ.ul', /debug
  mg_log, '%s, %f, %f, %f, %f', $
          wave_type, time, $
          occulter2.x, occulter2.y, occulter2.r, $
          name='flat.occ.lr', /debug

  mg_log, '%s, %d, %d, %d', $
          wave_type, $
          sxpar(primary_header, 'FOCUS'), $
          sxpar(primary_header, 'H-OCCULT'), $
          sxpar(primary_header, 'V-OCCULT'), $
          name='occulter', /debug

  ; P angles of post
  pang1 = sxpar(flat_header, 'POSTANG1')
  pang2 = sxpar(flat_header, 'POSTANG2')

  ; overlap P angle (from the field stop)
  delta_x = calc_occulter2.x - calc_occulter1.x + 1024.0 - nx
  delta_y = calc_occulter1.y - calc_occulter2.y + 1024.0 - ny
  overlap_angle = !radeg * atan(delta_y / delta_x)

  if (centering_diagnostics) then begin
    if (n_elements(current_l1_filename) gt 0L) then begin
      if (n_plus_beam gt 0) then begin
        bname = file_basename(current_l1_filename) + '.centering.ul.sav'
        save, occulter_points1, sub1, $
              filename=filepath(bname, $
                                subdir=comp_decompose_date(date_dir), $
                                root=engineering_dir)
      endif
      if (n_minus_beam gt 0) then begin
        bname = file_basename(current_l1_filename) + '.centering.lr.sav'
        save, occulter_points2, sub2, $
              filename=filepath(bname, $
                                subdir=comp_decompose_date(date_dir), $
                                root=engineering_dir)
      endif
    endif
  endif

  return, { occulter1: calc_occulter1, $
            occulter2: calc_occulter2, $
            field1: field1, $
            field2: field2, $
            post_angle1: pang1, $
            post_angle2: pang2, $
            delta_x: delta_x, $
            delta_y: delta_y, $
            overlap_angle: overlap_angle, $
            deltacenterx1: occulter1.x - calc_occulter1.x, $
            deltacentery1: occulter1.y - calc_occulter1.y, $
            deltacenterx2: occulter2.x - calc_occulter2.x, $
            deltacentery2: occulter2.y - calc_occulter2.y $
          }
end


; main-level example program

date = '20150418'
time = '092841'

comp_configuration, config_filename='../../config/comp.mgalloy.mahi.expan_factor_azi.cfg'
comp_initialize, date

@comp_config_common

filename = filepath(date + '.' + time + '.FTS', subdir=date, root=raw_basedir)
print, 'before processing...'
comp_l1_process_file, filename, date, '1074'
print, 'after processing...'

end
