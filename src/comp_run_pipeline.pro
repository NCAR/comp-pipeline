; docformat = 'rst'

;+
; Procedure to run all the routines in the CoMP pipeline.
;
; :Author:
;   Tomczyk modified by de Toma, Stanger, mgalloy
;
; :Keywords:
;   config_filename, in, optional, type=string
;     configuration filename to use, default is `comp.cfg` in the `src`
;     directory
;-
pro comp_run_pipeline, config_filename=config_filename
  compile_opt strictarr
  @comp_config_common
  @comp_testing_common

  _config_filename = file_expand_path(n_elements(config_filename) eq 0L $
                       ? filepath('comp.cfg', root=mg_src_root()) $
                       : config_filename)

  ;---------------  Initializing  --------------------------------

  t0 = systime(/seconds)
  catch, error
  if (error ne 0) then begin
    catch, /cancel
    t1 = systime(/seconds)
    mg_log, /last_error, name='comp', /critical
    mg_log, 'Total running time: %0.2f sec', t1 - t0, name='comp', /info

    if (n_elements(date_dir) gt 0) then begin
      if (lock_raw) then begin
        unlocked = comp_state(date_dir, /unlock)
        if (unlocked) then begin
          mg_log, 'Unlocked %s', filepath(date_dir, root=raw_basedir), $
                  name='comp', /info
        endif
      endif
    endif

    mg_log, /quit
    return
  endif

  loop_time = 0.

  start_memory = memory(/current)

  comp_configuration, config_filename=_config_filename
  comp_setup_loggers

  candidate_dirs = file_search(filepath(date_pattern, root=raw_basedir), $
                               /test_directory, $
                               count=n_candidate_dirs)
  candidate_dirs = file_basename(candidate_dirs)
  date_mask = stregex(candidate_dirs, $
                      '^[[:digit:]]{8}$', $
                      /boolean)
  date_dirs_ind = where(date_mask, n_dirs)

  if (n_candidate_dirs eq 0 || n_dirs eq 0) then begin
    t1 = systime(/seconds)
    mg_log, 'no days to process found in raw directory %s', $
            raw_basedir, $
            name='comp', /error
    mg_log, 'Total running time: %0.2f sec', t1 - t0, name='comp', /info
    mg_log, /quit
    return
  endif

  dirs = candidate_dirs[date_dirs_ind]

  ; ignore math errors
  orig_except = !except
  !except = 0

  for d = 0L, n_dirs - 1L do begin
    t0 = systime(/seconds)

    date_dir = dirs[d]

    comp_initialize, date_dir
    comp_setup_loggers_date, date_dir

    if (lock_raw) then begin
      available = comp_state(date_dir, /lock)
      if (available ne 1) then begin
        mg_log, '%s locked, skipping...', date_dir, name='comp', /info
        continue
      endif else begin
        mg_log, 'Locked %s', filepath(date_dir, root=raw_basedir), $
                name='comp', /info
      endelse
    endif

    mg_log, 'starting processing for %d', date_dir, name='comp', /info

    ;---------------  Prep  ----------------------------------------

    if (validate) then begin
      valid = comp_validator(date_dir)
      if (~valid) then begin
        mg_log, 'skipping %s...', date_dir, name='comp', /info
        if (lock_raw) then begin
          unlocked = comp_state(date_dir, /unlock)
          mg_log, 'Unlocked %s', filepath(date_dir, root=raw_basedir), $
                  name='comp', /info
        endif
        continue
      endif
    endif

    ;---------------  Level_1 data processing  ---------------------

    if (create_l1) then begin
      mg_log, 'starting level 1 processing for %s', date_dir, name='comp', /info
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      ; copy configuration file to the process output directory
      process_dir = filepath(date_dir, root=process_basedir)
      if (~file_test(process_dir, /directory)) then file_mkdir, process_dir

      l1_process_dir = filepath('level1', root=process_dir)
      if (~file_test(l1_process_dir, /directory)) then file_mkdir, l1_process_dir

      process_config_filename = filepath('comp.cfg', root=l1_process_dir)
      file_copy, _config_filename, process_config_filename, /overwrite

      ; take inventory of the data for this day
      mg_log, 'running file_type', name='comp', /info
      file_type_t0 = systime(/seconds)
      comp_file_type, date_dir
      file_type_t1 = systime(/seconds)
      mg_log, 'Total time for COMP_FILE_TYPE: %0.1f seconds', $
              file_type_t1 - file_type_t0, $
              name='comp', /debug
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      ; reduce bias images for this day
      mg_log, 'running dark mode', name='comp', /info
      comp_make_dark, date_dir, error=error
      if (error ne 0) then begin
        if (lock_raw) then begin
          unlocked = comp_state(date_dir, /unlock)
          mg_log, 'Unlocked %s', filepath(date_dir, root=raw_basedir), $
                  name='comp', /info
        endif
        continue
      endif
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      mg_log, 'running comp_make_flat', name='comp', /info
      make_flat_t0 = systime(/seconds)
      ; reduce opal images for this day
      comp_make_flat, date_dir, error=error
      make_flat_t1 = systime(/seconds)
      mg_log, 'Total time for COMP_MAKE_FLAT: %0.1f seconds', $
              make_flat_t1 - make_flat_t0, $
              name='comp', /debug
      if (error ne 0) then begin
        if (lock_raw) then begin
          unlocked = comp_state(date_dir, /unlock)
          mg_log, 'Unlocked %s', filepath(date_dir, root=raw_basedir), $
                  name='comp', /info
        endif
        continue
      endif
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      mg_log, 'running l1_process', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        l1_process_t0 = systime(/seconds)
        comp_l1_process, date_dir, process_wavelengths[w], error=error
        l1_process_t1 = systime(/seconds)
        mg_log, 'Total time for COMP_L1_PROCESS: %0.1f seconds', $
                l1_process_t1 - l1_process_t0, $
                name='comp', /debug
        if (error ne 0L) then continue
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      ; convert HST time in inventory files to UT; cannot be run before
      ; COMP_L1_PROCESS
      mg_log, 'running comp_update_filenames', name='comp', /info
      update_filenames_t0 = systime(/seconds)
      comp_update_filenames, date_dir
      update_filenames_t1 = systime(/seconds)
      mg_log, 'Total time for COMP_UPDATE_FILENAMES: %0.1f seconds', $
              update_filenames_t1 - update_filenames_t0, $
              name='comp', /debug
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      ; extract intensity images from Level_1 files
      mg_log, 'running comp_extract_intensity', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        extract_intensity_t0 = systime(/seconds)
        comp_extract_intensity, date_dir, process_wavelengths[w], error=error
        extract_intensity_t1 = systime(/seconds)
        mg_log, 'Total time for COMP_EXTRACT_INTENSITY: %0.1f seconds', $
                extract_intensity_t1 - extract_intensity_t0, $
                name='comp', /debug
        if (error ne 0) then continue
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      ; identify good data
      mg_log, 'running comp_gbu', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        gbu_t0 = systime(/seconds)
        comp_gbu, date_dir, process_wavelengths[w], error=error
        gbu_t1 = systime(/seconds)
        mg_log, 'Total time for COMP_GBU: %0.1f seconds', $
                gbu_t1 - gbu_t0, $
                name='comp', /debug
        if (error ne 0) then continue
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

    endif else begin
      mg_log, 'skipping L1 processing', name='comp', /info
    endelse

    if (distribute_l1) then begin
      mg_log, 'dstributing L1 data', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        comp_distribute_l1, date_dir, process_wavelengths[w]
      endfor
    endif else begin
      mg_log, 'skipping L1 distribution', name='comp', /info
    endelse

    ;---------------  Level_2 data processing  ---------------

    if (create_l2) then begin
      mg_log, 'starting level 2 processing for %s', date_dir, name='comp', /info

      process_dir = filepath(date_dir, root=process_basedir)
      if (~file_test(process_dir, /directory)) then file_mkdir, process_dir

      l2_process_dir = filepath('level2', root=process_dir)
      if (~file_test(l2_process_dir, /directory)) then file_mkdir, l2_process_dir

      process_config_filename = filepath('comp.cfg', root=l2_process_dir)
      file_copy, _config_filename, process_config_filename, /overwrite

      ; compute the mean, median and standard deviation of Level_1 data
      mg_log, 'running comp_average', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (process_wavelengths[w] ne '1083') then begin
          comp_average, date_dir, process_wavelengths[w], error=error
          if (error ne 0) then continue
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      ; perform 'quick' inversion
      mg_log, 'running comp_quick_invert', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (process_wavelengths[w] ne '1083') then begin
          comp_quick_invert, date_dir, process_wavelengths[w], error=error
          if (error ne 0) then continue
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      ; evaluate systematic errors in comp data
      mg_log, 'running comp_find_systematics', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (process_wavelengths[w] ne '1083') then begin
          comp_find_systematics, date_dir, process_wavelengths[w], 'mean', error=error
          if (error ne 0) then continue
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      mg_log, 'running 3-points analysis', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (process_wavelengths[w] ne '1083') then begin
          comp_l2_analytical_three, date_dir, process_wavelengths[w]
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (process_wavelengths[w] ne '1083') then begin
          comp_l2_create_jpgs, date_dir, process_wavelengths[w], nwl=3, /seq
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (process_wavelengths[w] ne '1083') then begin
          comp_l2_create_movies, date_dir, process_wavelengths[w], nwl=3
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      ;comp_quickview, date_dir, '1074', nwl=3,  dynamics=0
      ;comp_l2_analytical_three, date_dir, '1079'
      ;comp_l2_create_jpgs, date_dir, '1079', nwl=3, /seq
      ;comp_l2_create_movies, date_dir, '1079', nwl=3

      mg_log, 'running 5-points analysis', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (process_wavelengths[w] ne '1083') then begin
          comp_l2_analytical_five, date_dir, process_wavelengths[w]
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (process_wavelengths[w] ne '1083') then begin
          comp_l2_create_jpgs, date_dir, process_wavelengths[w], nwl=5, /seq
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (process_wavelengths[w] ne '1083') then begin
          comp_l2_create_movies, date_dir, process_wavelengths[w], nwl=5
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      mg_log, 'making daily summaries', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        comp_l2_summary,  date_dir, process_wavelengths[w]
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif else begin
      mg_log, 'skipping L2 processing', name='comp', /info
    endelse

    if (distribute_l2) then begin
      mg_log, 'dstributing L2 data', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        comp_distribute_l2, date_dir, process_wavelengths[w]
      endfor
    endif else begin
      mg_log, 'skipping L2 distribution', name='comp', /info
    endelse

    if (update_database) then begin
      mg_log, 'running comp_update_database', name='comp', /info
      db_t0 = systime(/seconds)
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        comp_update_database, date_dir, process_wavelengths[w]
      endfor
      db_t1 = systime(/seconds)
      mg_log, 'Total time for COMP_UPDATE_DATABASE: %0.1f seconds', $
              db_t1 - db_t0, $
              name='comp', /debug
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif

    t1 = systime(/seconds)
    mg_log, 'Total running time: %0.2f sec', t1 - t0, name='comp', /info

    if (lock_raw) then begin
      unlocked = comp_state(date_dir, /unlock)
      mg_log, 'Unlocked %s', filepath(date_dir, root=raw_basedir), $
              name='comp', /info
    endif
  endfor

  !except = orig_except

  mg_log, /quit
end
