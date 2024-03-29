; docformat = 'rst'

;+
; Combine the foreground and background (continuum) beams of CoMP, returning
; background-subtracted foreground and the background.
;
; :Uses:
;   comp_inventory_header, comp_get_component, comp_image_geometry,
;   comp_extract_beams, sxpar, sxaddpar, sxdelpar
;
; :Params:
;   images : in, required, type="fltarr(nx, ny, nimg)"
;     the array of images whos beams are to be combined
;   headers : in, required, type="strarr(ntags, nimg)"
;     headers corresponding to images
;   date_dir : in, required, type=string
;     the date directory for this data
;   images_combine : out, required, type="fltarr(620, 620, (2*np*nw))"
;     the combined images, one foreground, one background for each wavelength
;     and stokes component. The last index is sorted first by wavelength
;     (successive wavelengths are adjacent), then by polarization
;   headers_combine : out, optional, type="strarr(ntags2, (2*np*nw))"
;     headers corresponding to `images_combine`; the 'BEAM' tag, if present,
;     will be removed from the header; `ntags2` will therefore generally be
;     equal to `ntags - 1`
;
; :Keywords:
;   n_uniq_polstates : out, optional, type=long
;     set to a named variable to retrieve the number of unique
;     polarization states
;   n_uniq_wavelengths : out, optional, type=long
;     set to a named variable to retrieve the number of unique wavelengths
;   image_geometry : out, required, type=structure
;     image geometry specifications
;   wave_type : in, required, type=string
;     wavelength to process, '1074', '1079', etc.
;   uncorrected_images : in, optional, type="fltarr(nx, ny, n_images)"
;     the array of CoMP images, not flat corrected, but corrected in other ways
;   offsensor_mask : out, optional, type="bytarr(nx, ny)"
;     set to a named variable to retrieve the mask of off-sensor pixels; 1 is
;     on-sensor, 1 if off-sensor
;
; :Author:
;   MLSO Software Team
;-
pro comp_combine_beams, images, headers, date_dir, $
                        images_combine, headers_combine, primary_header, $
                        n_uniq_polstates=np, n_uniq_wavelengths=nw, $
                        image_geometry=image_geometry, $
                        uncorrected_geometry=uncorrected_geometry, $
                        wave_type=wave_type, $
                        uncorrected_images=uncorrected_images, $
                        offsensor_mask=offsensor_mask, $
                        error=error
  compile_opt strictarr
  @comp_constants_common
  @comp_config_common
  @comp_check_common
  @comp_diagnostics_common

  comp_inventory_header, headers, beam, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret

  uwave = wave[uniq(wave, sort(wave))]  ; unique waves
  upol = pol[uniq(pol, sort(pol))]      ; unique polarizations
  nw = n_elements(uwave)
  np = n_elements(upol)

  ntags = n_elements(headers[*, 0])
  if (sxpar(headers[*, 0], 'BEAM') ne 0) then ntags--

  ntags += 6   ; for geometry keywords for flats
  ntags += 2   ; expand FLAT_EXT/FLAT_MED to versions 1 and 2

  ; output image and header array
  images_combine = dblarr(nx, ny, 2 * np * nw)
  headers_combine = strarr(ntags, 2 * np * nw)

  plus_indices = where(beam gt 0, n_plus_images, $
                       complement=minus_indices, $
                       ncomplement=n_minus_images)

  plus_images = images[*, *, plus_indices]
  plus_headers = headers[*, plus_indices]
  minus_images = images[*, *, minus_indices]
  minus_headers = headers[*, minus_indices]

  image_geometry = comp_image_geometry(uncorrected_images, headers, date_dir, $
                                       primary_header=primary_header, $
                                       uncorrected_geometry=uncorrected_geometry, $
                                       wave_type=wave_type, $
                                       error=error)

  n_bad_post_angle = abs(image_geometry.post_angle1 $
                           - image_geometry.post_angle2) $
                       gt post_angle_diff_tolerance
  if (n_bad_post_angle gt 0L) then begin
    mg_log, 'post angle diff > %0.1f deg: %0.1f and %0.1f', $
            post_angle_diff_tolerance, $
            image_geometry.post_angle1, $
            image_geometry.post_angle2, $
            name='comp', /debug
  endif

  case wave_type of
    '1074': n_1074_files_post_angle_diff += n_bad_post_angle
    '1079': n_1079_files_post_angle_diff += n_bad_post_angle
    '1083': n_1083_files_post_angle_diff += n_bad_post_angle
  endcase

  offsensor_mask = bytarr(nx, ny) + 1B

  for i = 0L, np - 1L do begin   ; loop over unique polarizations
    for j = 0L, nw - 1L do begin   ; loop over unique wavelengths
      ; get the two beam states for this wavelength and polarization
      imgplus = comp_get_component(images, headers, upol[i], 1, uwave[j], $
                                   headersout=hplus, /noskip)
      imgminus = comp_get_component(images, headers, upol[i], -1, uwave[j], $
                                    headersout=hminus, /noskip)

      ; extract the foreground and background subimages from both
      comp_extract_beams, imgplus, hplus, date_dir, bgplus, fgplus, $
                          image_geometry=image_geometry, $
                          uncorrected_geometry=uncorrected_geometry, $
                          offsensor_mask=offsensor_mask_plus
      comp_extract_beams, imgminus, hminus, date_dir, fgminus, bgminus, $
                          image_geometry=image_geometry, $
                          uncorrected_geometry=uncorrected_geometry, $
                          offsensor_mask=offsensor_mask_minus

      temporary_intermediate_product = 0B
      if (temporary_intermediate_product) then begin
        dt = strmid(file_basename(current_l1_filename), 0, 15)
        basename = string(dt, i, j, format='%s.comp.pre-combine.p%d.w%d.sav')
        filename = filepath(basename, $
                            subdir=comp_decompose_date(date_dir), $
                            root=engineering_dir)
        save, fgplus, fgminus, bgplus, bgminus, filename=filename
      endif

      offsensor_mask and= offsensor_mask_plus and offsensor_mask_minus

      ; foreground part (with background subtracted)
      nonzero = (fgplus ne 0.0) + (fgminus ne 0.0)  ; 0.0's are missing (off detector)
      ; create mask for off detector from zeros of `nonzero`
      nonzero >= 1.0                                ; don't divide by 0

      if (wave_type eq '1083') then begin
        ; NOTE: the He background is contaminated, so don't subtract. Also,
        ;       finding the center is harder for He, so using only a single beam
        ;       makes the result sharper
        images_combine[*, *, i * nw + j] = fgminus
      endif else if (~subtract_background) then begin
        images_combine[*, *, i * nw + j] = (fgplus + fgminus) / nonzero
      endif else begin
        images_combine[*, *, i * nw + j] = (fgplus - bgplus +  fgminus - bgminus) / nonzero
      endelse

      ; background part
      images_combine[*, *, np * nw + i * nw + j] = 0.5 * (bgminus + bgplus)

      hcombined = comp_combine_headers([[hplus], [hminus]], [0, 1])

      ; update the headers
      sxdelpar, hcombined, 'BEAM'
      sxaddpar, hcombined, 'NAVERAGE', $
                sxpar(hplus, 'NAVERAGE') + sxpar(hminus, 'NAVERAGE')

      sxaddpar, hcombined, 'OXCNTER1', image_geometry.flat_occulter1.x + 1.0, $
                ' Occulter center X for dist corrected sub-flat1', $
                format='(F0.3)'
      sxaddpar, hcombined, 'OYCNTER1', image_geometry.flat_occulter1.y + 1.0, $
                ' Occulter center Y for dist corrected sub-flat1', $
                format='(F0.3)'
      sxaddpar, hcombined, 'ORADIUS1', image_geometry.flat_occulter1.r, $
                ' Occulter radius for dist corrected sub-flat1', $
                format='(F0.3)'

      sxaddpar, hcombined, 'OXCNTER2', image_geometry.flat_occulter2.x + 1.0, $
                ' Occulter center X for dist corrected sub-flat2', $
                format='(F0.3)'
      sxaddpar, hcombined, 'OYCNTER2', image_geometry.flat_occulter2.y + 1.0, $
                ' Occulter center Y for dist corrected sub-flat2', $
                format='(F0.3)'
      sxaddpar, hcombined, 'ORADIUS2', image_geometry.flat_occulter2.r, $
                ' Occulter radius for dist corrected sub-flat2', $
                format='(F0.3)'

      ; keep both FLATEXT/FLATMED values

      flatext1 = sxpar(hplus, 'FLATEXT', comment=comment1)
      flatext2 = sxpar(hminus, 'FLATEXT', comment=comment2)
      sxdelpar, hcombined, 'FLATEXT'
      sxaddpar, hcombined, 'FLATEXT1', flatext1, comment1, after='FLATFILE'
      sxaddpar, hcombined, 'FLATEXT2', flatext2, comment2, after='FLATEXT1'

      flatmed1 = sxpar(hplus, 'FLATMED', comment=comment1)
      flatmed2 = sxpar(hminus, 'FLATMED', comment=comment2)
      sxdelpar, hcombined, 'FLATMED'
      sxaddpar, hcombined, 'FLATMED1', flatmed1, comment1, format='(F0.2)', after='FLATEXT2'
      sxaddpar, hcombined, 'FLATMED2', flatmed2, comment2, format='(F0.2)', after='FLATMED1'

      headers_combine[0, i * nw + j] = reform(hcombined, n_elements(hcombined), 1)
      sxaddpar, hcombined, 'POLSTATE', 'BKG' + upol[i], ' Polarization state'

      headers_combine[0, np * nw + i * nw + j] = reform(hcombined, n_elements(hcombined), 1)
    endfor
  endfor
end
