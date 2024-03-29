; docformat = 'rst'

;+
; Procedure to determine the location of the occulting disk and field stop in
; each beam of a CoMP large image and put them into a FITS header.
;
; :Uses:
;   comp_constants_common, comp_find_annulus, comp_find_post, comp_extract1,
;   comp_extract2, mkhdr, sxaddpar, comp_apply_distortion
;
; :Params:
;   image : in
;     the large format comp image
;   header : out
;     the resulting FITS header
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   uncorrected_occulter1 : out
;     structure containing the parameters of the occulting disk `{x,y,radius}`
;     for distortion uncorrected subimage 1 (pixels)
;   uncorrected_field1 : out
;     structure containing the parameters of the field stop `{x,y,radius}` for
;     distortion uncorrected subimage 1 (pixels)
;   uncorrected_post_angle1 : out
;     the position angle of the post for distortion uncorrected subimage 1 (degrees)
;   uncorrected_occulter2 : out
;     structure containing the parameters of the occulting disk `{x,y,radius}`
;     for distortion uncorrected subimage 2 (pixels)
;   uncorrected_field2 : out
;     structure containing the parameters of the field stop `{x,y,radius}` for
;     distortion uncorrected subimage 2 (pixels)
;   uncorrected_post_angle2 : out
;     the position angle of the post for distortion uncorrected subimage 2 (degrees)
;
; :Keywords:
;   error : out, optional, type=long
;     error status of making the header, 0 for success, anything else indicates
;     an error
;
; :Author:
;   MLSO Software Team
;
; :History:
;   added comments - 10/23/14 ST
;   see git log for recent changes
;-
pro comp_make_header, image, header, date_dir, $
                      occulter_radius_guess, $
                      uncorrected_occulter1, $
                      uncorrected_field1, $
                      uncorrected_post_angle1, $
                      uncorrected_occulter2, $
                      uncorrected_field2, $
                      uncorrected_post_angle2, $
                      error=error
  compile_opt strictarr
  @comp_constants_common
  @comp_config_common
  @comp_diagnostics_common
  @comp_mask_constants_common

  ; set to add other occulter radius (R-2) to headers, both distortion
  ; corrected and not
  elliptical_occulter = 0B

  mkhdr, header, image, /image

  ; retrieve distortion coefficients in file: dx1_c, dy1_c, dx2_x, dy2_c
  restore, filename=filepath(distortion_coeffs_file, root=binary_dir)
 
  flat1 = comp_extract1(image)   ; extract the subimage
  flat2 = comp_extract2(image)   ; extract the subimage

  ; we need to find the occulter/field centers for both the distortion corrected
  ; and uncorrected flats
  uncorrected_flat1 = flat1
  uncorrected_flat2 = flat2

  case distortion_method of
    'coeffs': begin
        k1 = distortion_coeffs[0]
        k2 = distortion_coeffs[1]
      end
    'file': begin
        ; retrieve distortion coefficients in file: dx1_c, dy1_c, dx2_x, dy2_c
        restore, filename=filepath(distortion_coeffs_file, root=binary_dir)
      end
    else:
  endcase

  ; remove distortion (NOTE: these images will not be saved!)
  case distortion_method of
    'coeffs': begin
        flat1 = comp_apply_distortion_coeffs(flat1, k1)
        flat2 = comp_apply_distortion_coeffs(flat2, k2)
      end
    'file': begin
        flat1 = comp_apply_distortion_file(flat1, dx1_c, dy1_c)
        flat2 = comp_apply_distortion_file(flat2, dx2_c, dy2_c)
      end
    else:
  endcase

  uncorrected_occulter_guess1 = comp_find_flat_initial_guess(uncorrected_flat1)
  uncorrected_occulter_guess2 = comp_find_flat_initial_guess(uncorrected_flat2)
  corrected_occulter_guess1   = comp_find_flat_initial_guess(flat1)
  corrected_occulter_guess2   = comp_find_flat_initial_guess(flat2)

  ; image 1
  comp_find_annulus, flat1, occulter1, field1, $
                     occulter_guess=[corrected_occulter_guess1, occulter_radius_guess], $
                     field_guess=[corrected_occulter_guess1, field_radius_guess], $
                     occulter_points=corrected_occulter_points1, $
                     field_points=corrected_field_points1, $
                     elliptical=elliptical_occulter, $
                     error=error
  if (error ne 0L) then begin
    mg_log, 'error finding image center', name='comp', /warn
    return
  endif

  comp_find_post, flat1, occulter1, field1, post_angle1

  comp_find_annulus, uncorrected_flat1, $
                     uncorrected_occulter1, $
                     uncorrected_field1, $
                     occulter_guess=[uncorrected_occulter_guess1, occulter_radius_guess], $
                     field_guess=[uncorrected_occulter_guess1, field_radius_guess], $
                     occulter_points=uncorrected_occulter_points1, $
                     field_points=uncorrected_field_points1, $
                     elliptical=elliptical_occulter
                     error=error
  if (error ne 0L) then begin
    mg_log, 'error finding image center', name='comp', /warn
    return
 endif

  comp_find_post, uncorrected_flat1, $
                  uncorrected_occulter1, uncorrected_field1, $
                  uncorrected_post_angle1

  ; image 2
  comp_find_annulus, flat2, occulter2, field2, $
                     occulter_guess=[corrected_occulter_guess2, occulter_radius_guess], $
                     field_guess=[corrected_occulter_guess2, field_radius_guess], $
                     occulter_points=corrected_occulter_points2, $
                     field_points=corrected_field_points2, $
                     elliptical=elliptical_occulter, $
                     error=error
  if (error ne 0L) then begin
    mg_log, 'error finding image center', name='comp', /warn
    return
  endif

 comp_find_post, flat2, occulter2, field2, post_angle2

 comp_find_annulus,  uncorrected_flat2, $
                     uncorrected_occulter2, $
                     uncorrected_field2, $
                     occulter_guess=[uncorrected_occulter_guess2, occulter_radius_guess], $
                     field_guess=[uncorrected_occulter_guess2, field_radius_guess], $
                     occulter_points=uncorrected_occulter_points2, $
                     field_points=uncorrected_field_points2, $
                     elliptical=elliptical_occulter, $
                     error=error
  if (error ne 0L) then begin
    mg_log, 'error finding image center', name='comp', /warn
    return
  endif

  comp_find_post,  uncorrected_flat2, $
                   uncorrected_occulter2, uncorrected_field2, $
                   uncorrected_post_angle2

  if (eng_flat_gifs) then begin
    comp_write_annotated_flat_gif, flat1, occulter1, field1, $
                                   filename=filepath(current_flatname + '.flat1.gif', $
                                                     subdir=comp_decompose_date(date_dir), $
                                                     root=engineering_dir)
    comp_write_annotated_flat_gif, flat2, occulter2, field2, $
                                   filename=filepath(current_flatname + '.flat2.gif', $
                                                     subdir=comp_decompose_date(date_dir), $
                                                     root=engineering_dir)
  endif

  if (centering_diagnostics) then begin
    save, corrected_occulter_points1, $
          corrected_occulter_points2, $
          corrected_field_points1, $
          corrected_field_points2, $
          uncorrected_occulter_points1, $
          uncorrected_occulter_points2, $
          uncorrected_field_points1, $
          uncorrected_field_points2, $
          filename=filepath(current_flatname + '.centering-flat.sav', $
                            subdir=comp_decompose_date(date_dir), $
                            root=engineering_dir)
  endif

  ; write occulter position
  ; add 1 to adopt FITS standard
  ; convert 620x620 geometry to 1024x1024 geometry
  sxaddpar, header, 'OXCNTER1', occulter1.x + 1.0, $
            ' Occulter center X for dist corrected sub-image 1'
  sxaddpar, header, 'OYCNTER1', occulter1.y + 1.0 + 1024 - ny, $
            ' Occulter center Y for dist corrected sub-image 1'
  sxaddpar, header, 'ORADIUS1', occulter1.r, $
            ' Occulter radius for dist corrected sub-image 1'
  if (keyword_set(elliptical_occulter)) then begin
    sxaddpar, header, 'ORAD1-2', occulter1.r2, $
              ' Occulter radius for dist corrected sub-image 1'
    sxaddpar, header, 'OTILT1', occulter1.tilt, $
              ' tilt of ellipse for dist corrected sub-image 1'
  endif
  sxaddpar, header, 'OXCNTER2', occulter2.x + 1.0 + 1024 - nx, $
            ' Occulter center X for dist corrected sub-image 2'
  sxaddpar, header, 'OYCNTER2', occulter2.y + 1.0, $
            ' Occulter center Y for dist corrected sub-image 2'
  sxaddpar, header, 'ORADIUS2', occulter2.r, $
            ' Occulter radius for dist corrected sub-image 2'
  if (keyword_set(elliptical_occulter)) then begin
    sxaddpar, header, 'ORAD2-2', occulter2.r2, $
              ' Occulter radius for dist corrected sub-image 2'
    sxaddpar, header, 'OTILT2', occulter1.tilt, $
              ' tilt of ellipse for dist corrected sub-image 2'
  endif

  sxaddpar, header, 'OXCNTRU1', uncorrected_occulter1.x + 1.0, $
            ' Occulter center X for dist uncorrected sub-image 1'
  sxaddpar, header, 'OYCNTRU1', uncorrected_occulter1.y + 1.0 + 1024 - ny, $
            ' Occulter center Y for dist uncorrected sub-image 1'
  sxaddpar, header, 'ORADU1',   uncorrected_occulter1.r, $
            ' Occulter radius for dist uncorrected sub-image 1'
  if (keyword_set(elliptical_occulter)) then begin
    sxaddpar, header, 'ORADU1-2', uncorrected_occulter1.r2, $
              ' Occulter radius for dist uncorrected sub-image 1'
    sxaddpar, header, 'OTILTU1', uncorrected_occulter1.tilt, $
              ' tilt of ellipse for dist uncorrected sub-image 1'
  endif
  sxaddpar, header, 'OXCNTRU2', uncorrected_occulter2.x + 1.0 + 1024 - nx, $
            ' Occulter center X for dist uncorrected sub-image 2'
  sxaddpar, header, 'OYCNTRU2', uncorrected_occulter2.y + 1.0, $
            ' Occulter center Y for dist uncorrected sub-image 2'
  sxaddpar, header, 'ORADU2',   uncorrected_occulter2.r, $
            ' Occulter radius for dist uncorrected sub-image 2'
  if (keyword_set(elliptical_occulter)) then begin
    sxaddpar, header, 'ORADU2-2', uncorrected_occulter2.r2, $
              ' Occulter radius for dist uncorrected sub-image 2'
    sxaddpar, header, 'OTILTU2', uncorrected_occulter2.tilt, $
              ' tilt of ellipse for dist uncorrected sub-image 2'
  endif

  ; field position
  sxaddpar, header, 'FXCNTER1', field1.x + 1.0, $
            ' Field stop center X for dist corrected sub-image 1'
  sxaddpar, header, 'FYCNTER1', field1.y + 1.0 + 1024 - ny, $
            ' Field stop center Y for dist corrected sub-image 1'
  sxaddpar, header, 'FRADIUS1', field1.r, $
            ' Field stop radius for dist corrected sub-image 1'
  sxaddpar, header, 'FXCNTER2', field2.x + 1.0 + 1024 - nx, $
            ' Field stop center X for dist corrected sub-image 2'
  sxaddpar, header, 'FYCNTER2', field2.y + 1.0, $
            ' Field stop center Y for dist corrected sub-image 2'
  sxaddpar, header, 'FRADIUS2', field2.r, $
            ' Field stop radius for dist corrected sub-image 2'

  sxaddpar, header, 'FXCNTRU1', uncorrected_field1.x + 1.0, $
            ' Field stop center X for distortion uncorrected sub-image 1'
  sxaddpar, header, 'FYCNTRU1', uncorrected_field1.y + 1.0 + 1024 - ny, $
            ' Field stop center Y for dist uncorrected sub-image 1'
  sxaddpar, header, 'FRADU1',   uncorrected_field1.r, $
            ' Field stop radius for dist uncorrected sub-image 1'
  sxaddpar, header, 'FXCNTRU2', uncorrected_field2.x + 1.0 + 1024 - nx,  $
            ' Field stop center X for dist uncorrected sub-image 2'
  sxaddpar, header, 'FYCNTRU2', uncorrected_field2.y + 1.0, $
            ' Field stop center Y for dist uncorrected sub-image 2'
  sxaddpar, header, 'FRADU2',   uncorrected_field2.r, $
            ' Field stop radius for dist uncorrected sub-image 2'

  ; position angles for post
  sxaddpar, header, 'POSTANG1', post_angle1, $
            ' Position angle of post for dist corrected sub-image 1', $
            format='(F0.3)'
  sxaddpar, header, 'POSTANG2', post_angle2, $
            ' Position angle of post for dist corrected sub-image 2', $
            format='(F0.3)'

  sxaddpar, header, 'PSTANGU1', uncorrected_post_angle1, $
            ' Position angle of post for dist uncorrected sub-image 1', $
            format='(F0.3)'
  sxaddpar, header, 'PSTANGU2', uncorrected_post_angle2, $
            ' Position angle of post for dist uncorrected sub-image 2', $
            format='(F0.3)'
end
