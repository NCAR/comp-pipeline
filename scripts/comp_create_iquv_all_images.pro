; main-level example program

;+
; Create the "IQUV all" images for dates that have already had their level 1
; processing complete.
;
; :Keywords:
;   config_filename, in, required, type=string
;     configuration filename to use
;-
pro comp_create_iquv_all_images, config_filename=config_filename
  compile_opt strictarr
  @comp_config_common
  @comp_diagnostics_common

  comp_configuration, config_filename=config_filename

  comp_setup_loggers

  candidate_dates = comp_expand_date_expr(date_pattern, count=n_candidate_dates)
  if (n_candidate_dates eq 0) then begin
    t1 = systime(/seconds)
    mg_log, 'no days to process', name='comp', /error
    mg_log, 'total running time: %0.2f sec', t1 - t0, name='comp', /info
    mg_log, /quit
    return
  endif

  for d = 0L, n_candidate_dates - 1L do begin
    date = candidate_dates[d]

    comp_setup_loggers_date, date
    comp_update_configuration, date
    comp_initialize, date

    for w = 0L, n_elements(process_wavelengths) - 1L do begin
      l1_filenames = comp_find_l1_file(date, process_wavelengths[w], $
                                       /all, $
                                       count=n_l1_files)
      for f = 0L, n_elements(l1_filenames) - 1L do begin
        comp_write_all_iquv_image, date, $
                                   process_wavelengths[w], $
                                   l1_filenames[f]
      endfor
    endfor
  endfor

  done:
  mg_log, /quit
end


; main-level example program

config_basename = 'comp.reprocess-check-2011.cfg'
config_filename = filepath(config_basename, $
                           subdir=['..', 'config'], $
                           root=mg_src_root())

comp_create_iquv_all_images, config_filename=config_filename

end
