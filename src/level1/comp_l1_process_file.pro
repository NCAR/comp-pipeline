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
;   comp_constants_common, comp_mask_constants_common
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
;   Joseph Plowman
;-
pro comp_l1_process_file, filename, date_dir, wave_type
  compile_opt strictarr

  @comp_constants_common
  @comp_mask_constants_common

  comp_read_data, filename, images, headers, header0
  comp_apply_flats_darks, images, headers, date_dir
  comp_demodulate, images, headers, images_demod, headers_demod
  comp_inventory_header, headers_demod, beam, group, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret

  ; depending on which polarizations are present, call the appropriate
  ; cross-talk correction routines

  if (total(pol eq 'V') gt 0) then begin
    mg_log, 'fixing V crosstalk', name='comp/l1_process', /info
    comp_fix_vxtalk, date_dir, images_demod, headers_demod, filename
  endif

  if (total(pol eq 'Q') gt 0 or total(pol eq 'U') gt 0) then begin
    mg_log, 'fixing QU crosstalk', name='comp/l1_process', /info
    comp_fix_quxtalk, date_dir, images_demod, headers_demod, filename
  endif

  ; split the foreground (on-band) and background (continuum) beams into
  ; separate images, and subtract the backgrounds from the foregrounds. Store
  ; each into its own set of images with updated headers.
  comp_combine_beams, images_demod, headers_demod, date_dir, $
                      images_combine, headers_combine, $
                      n_uniq_polstates=np, n_uniq_wavelengths=nw

  ; double precision not required in output
  images_combine = float(images_combine)

  ; update the primary header and write the processed data to the output file
  comp_promote_primary_header_l1, headers, header0, date_dir
  comp_write_processed, images_combine, headers_combine, header0, date_dir, $
                        filename, wave_type
end
