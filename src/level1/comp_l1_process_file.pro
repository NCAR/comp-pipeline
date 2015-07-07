; docformat = 'rst'

;+
; Process a CoMP raw file, and save it to the specified output file as Level
; 1. Applies flats and darks, demodulates, fixes crosstalk, and combines the
; CoMP beams.
;
; :Uses:
;   comp_read_data, comp_flats_darks, comp_demodulate, comp_inventory_header,
;   comp_fix_vxtalk, comp_fix_quxtalk, comp_combine_beams,
;   comp_promote_primary_header_l1, comp_write_processed,
;   comp_constants_common, comp_mask_constants_common
;
; :Params:
;   infile : in, required, type=string
;     the input file to process
;   outfile : in, required, type=string
;     the output file where the processed level 1 data will be written
;   date_dir : in, required, type=string
;     the date directory of the input and output files
;
; :Author:
;   Joseph Plowman
;-
pro comp_l1_process_file, infile, outfile, date_dir
  compile_opt strictarr

  tstart = systime(1)

  @comp_constants_common
  @comp_mask_constants_common

  comp_read_data, infile, images, headers, header0   ; read the input file
  comp_flats_darks, images, headers, date_dir        ; apply flats and darks
  comp_demodulate, images, headers, images_demod, headers_demod   ; demodulate
  comp_inventory_header, headers_demod, beam, group, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret

  ; depending on which polarizations are present, call the appropriate
  ; cross-talk correction routines
  if (total(pol eq 'V') gt 0) then begin
    comp_fix_vxtalk, date_dir, images_demod, headers_demod, infile
  endif
  if (total(pol eq 'Q') gt 0 or total(pol eq 'U') gt 0) then begin
    comp_fix_quxtalk, date_dir, images_demod, headers_demod, infile
  endif

  ; split the foreground (on-band) and background (continuum) beams into
  ; separate images, and subtract the backgrounds from the foregrounds. Store
  ; each into its own set of images with updated headers.
  comp_combine_beams, images_demod, headers_demod, date_dir, $
                      images_combine, headers_combine

  ; update the primary header and write the now processed data to the output
  ; file
  comp_promote_primary_header_l1, headers, header0, date_dir
  comp_write_processed, images_combine, headers_combine, header0, date_dir, $
                        outfile

  print, systime(1) - tstart
end
