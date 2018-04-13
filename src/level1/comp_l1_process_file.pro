; docformat = 'rst'

;+
; Process a CoMP raw file, and save it to the specified output file as Level
; 1. Applies flats and darks, demodulates, fixes crosstalk, and combines the
; CoMP beams.
;
; :Uses:
;   comp_read_data, comp_apply_flats_darks, comp_demodulate,
;   comp_inventory_header, comp_fix_vxtalk, comp_fix_quxtalk,
;   comp_combine_beams, comp_promote_primary_header_l1, comp_write_processed,
;   comp_constants_common, comp_mask_constants_common,
;   comp_heliographic_correction
;
; :Params:
;   filename : in, required, type=string
;     the input file to process
;   date_dir : in, required, type=string
;     the date directory of the input and output files
;   wave_type : in, required, type=string
;     wavelength to process, '1074', '1079', etc.
;
; :Author:
;   MLSO Software Team
;-
pro comp_l1_process_file, filename, date_dir, wave_type
  compile_opt strictarr

  @comp_constants_common
  @comp_mask_constants_common
  @comp_config_common
  @comp_diagnostics_common
  current_l1_filename = filename

  comp_read_data, filename, images, headers, header0

  comp_apply_flats_darks, wave_type, images, headers, header0, date_dir, error=error, $
                          uncorrected_images=uncorrected_images
  if (error ne 0L) then begin
    mg_log, 'skipping %s (error applying flats/darks)', $
            file_basename(filename), name='comp', /warn
    return
  endif

  if (flat_corrected_output) then begin
    comp_write_intermediate, header0, images, headers, wave_type, date_dir, $
                             filename, 'flatcor'
  endif

  ; TODO: do uncorrected_images need to be demodulated and corrected for
  ; crosstalk?

  comp_demodulate, images, headers, images_demod, headers_demod

  if (demodulated_output) then begin
    comp_write_intermediate, header0, images_demod, headers_demod, $
                             wave_type, date_dir, $
                             filename, 'demod'
  endif

  comp_inventory_header, headers_demod, beam, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret

  ; depending on which polarizations are present, call the appropriate
  ; cross-talk correction routines
  if (correct_crosstalk) then begin
    if (total(pol eq 'V') gt 0) then begin
      mg_log, 'fixing V crosstalk', name='comp', /debug
      comp_fix_vxtalk, wave_type, date_dir, images_demod, headers_demod, filename
    endif

    if (total(pol eq 'Q') gt 0 or total(pol eq 'U') gt 0) then begin
      mg_log, 'fixing QU crosstalk', name='comp', /debug
      comp_fix_quxtalk, date_dir, images_demod, headers_demod, filename
    endif
  endif

  ; split the foreground (on-band) and background (continuum) beams into
  ; separate images, and subtract the backgrounds from the foregrounds. Store
  ; each into its own set of images with updated headers.
  comp_combine_beams, images_demod, headers_demod, date_dir, $
                      images_combine, headers_combine, header0, $
                      n_uniq_polstates=np, n_uniq_wavelengths=nw, $
                      image_geometry=image_geometry, $
                      wave_type=wave_type, $
                      uncorrected_images=uncorrected_images

  ; double precision not required in output
  images_combine = float(images_combine)

  ; update the primary header and write the processed data to the output file
  comp_promote_primary_header_l1, headers, header0, date_dir, wave_type, $
                                  image_geometry=image_geometry, $
                                  headers_combine=headers_combine

  ; perform heliographic coordinate transformation
  if (perform_polarimetric_transform) then begin
    p_angle = sxpar(header0, 'SOLAR_P0')
    overlap_angle = sxpar(header0, 'OVRLPANG')
    comp_polarimetric_correction, images_combine, headers_combine, $
                                  p_angle, overlap_angle
  endif

  comp_write_processed, images_combine, headers_combine, header0, date_dir, $
                        filename, wave_type
end


; main-level example program

date = '20150925'

config_filename = filepath('comp.mgalloy.mahi.destray.cfg', $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())
comp_configuration, config_filename=config_filename
comp_initialize, date

@comp_config_common

filename = filepath('20150925.112236.FTS', subdir=date, root=raw_basedir)
comp_l1_process_file, filename, date, '1074'

end
