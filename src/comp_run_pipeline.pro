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

    error = 0L
    date_dir = dirs[d]

    comp_initialize, date_dir
    if (~dry_run) then comp_setup_loggers_date, date_dir

    if (lock_raw) then begin
      available = comp_state(date_dir)
      if (available ne 1) then begin
        mg_log, '%s locked, skipping...', date_dir, name='comp', /info
        continue
      endif else begin
        if (~dry_run) then begin
          available = comp_state(date_dir, /lock)
          mg_log, 'Locked %s', filepath(date_dir, root=raw_basedir), $
                  name='comp', /info
        endif
      endelse
    endif

    mg_log, 'starting processing for %d', date_dir, name='comp', /info

    version = comp_find_code_version(revision=revision, branch=branch)
    mg_log, 'running comp-pipeline %s (%s on %s)', version, revision, branch, $
            name='comp', /debug
    mg_log, 'using IDL %s on %s', !version.release, !version.os_name, $
            name='comp', /debug

    ;---------------  Prep  ----------------------------------------

    if (validate) then begin
      mg_log, 'validating raw data', name='comp', /info
      if (~dry_run) then begin
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
    endif

    ;---------------  Level_1 data processing  ---------------------

    if (create_l1 || create_flatsdarks) then begin
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

      ; write constants used for given day to engineering directory
      comp_write_epochs, date_dir

      ; take inventory of the data for this day
      mg_log, 'determining file types', name='comp', /info
      file_type_t0 = systime(/seconds)
      if (~dry_run) then comp_file_type, date_dir
      file_type_t1 = systime(/seconds)
      mg_log, 'Total time for COMP_FILE_TYPE: %0.1f seconds', $
              file_type_t1 - file_type_t0, $
              name='comp', /debug
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif

    if (create_flatsdarks) then begin
      ; reduce bias images for this day
      mg_log, 'making darks', name='comp', /info
      if (~dry_run) then comp_make_dark, date_dir, error=error
      if (error ne 0) then begin
        if (lock_raw) then begin
          unlocked = comp_state(date_dir, /unlock)
          mg_log, 'Unlocked %s', filepath(date_dir, root=raw_basedir), $
                  name='comp', /info
          processed = comp_state(date_dir, /processed)
          mg_log, 'Marked %s as processed', filepath(date_dir, root=raw_basedir), $
                  name='comp', /info
        endif
        continue
      endif
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      mg_log, 'making flats', name='comp', /info
      make_flat_t0 = systime(/seconds)
      ; reduce opal images for this day
      if (~dry_run) then comp_make_flat, date_dir, error=error
      make_flat_t1 = systime(/seconds)
      mg_log, 'Total time for COMP_MAKE_FLAT: %0.1f seconds', $
              make_flat_t1 - make_flat_t0, $
              name='comp', /debug
      if (error ne 0) then begin
        if (lock_raw) then begin
          unlocked = comp_state(date_dir, /unlock)
          mg_log, 'Unlocked %s', filepath(date_dir, root=raw_basedir), $
                  name='comp', /info
          processed = comp_state(date_dir, /processed)
          mg_log, 'Marked %s as processed', filepath(date_dir, root=raw_basedir), $
                  name='comp', /info
        endif
        continue
      endif
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif

    if (create_l1) then begin
      mg_log, 'running l1_process', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        l1_process_t0 = systime(/seconds)
        if (~dry_run) then begin
          comp_l1_process, date_dir, process_wavelengths[w], error=error
        endif
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
      mg_log, 'updating filenames to UT', name='comp', /info
      update_filenames_t0 = systime(/seconds)
      if (~dry_run) then comp_update_filenames, date_dir
      update_filenames_t1 = systime(/seconds)
      mg_log, 'Total time for COMP_UPDATE_FILENAMES: %0.1f seconds', $
              update_filenames_t1 - update_filenames_t0, $
              name='comp', /debug
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      ; extract intensity images from Level_1 files
      mg_log, 'extracting intensity', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        extract_intensity_t0 = systime(/seconds)
        if (~dry_run) then begin
          comp_extract_intensity, date_dir, process_wavelengths[w], error=error
        endif
        extract_intensity_t1 = systime(/seconds)
        mg_log, 'Total time for COMP_EXTRACT_INTENSITY: %0.1f seconds', $
                extract_intensity_t1 - extract_intensity_t0, $
                name='comp', /debug
        if (error ne 0) then continue
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif

    if (perform_gbu) then begin
      ; identify good data
      mg_log, 'determining GBU', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        gbu_t0 = systime(/seconds)
        if (~dry_run) then begin
          comp_gbu, date_dir, process_wavelengths[w], error=error
        endif
        gbu_t1 = systime(/seconds)
        mg_log, 'Total time for COMP_GBU: %0.1f seconds', $
                gbu_t1 - gbu_t0, $
                name='comp', /debug
        if (error ne 0) then continue
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif

    if (check_l1) then begin
      ; check metrics of final L1 data
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        mg_log, 'checking %s L1 data', process_wavelengths[w], $
                name='comp', /info
        check_l1_t0 = systime(/seconds)
        if (~dry_run) then begin
          comp_l1_check, date_dir, process_wavelengths[w]
        endif
        check_l1_t1 = systime(/seconds)
        mg_log, 'Total time for COMP_L1_CHECK: %0.1f seconds', $
                check_l1_t1 - check_l1_t0, $
                name='comp', /debug
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif

    if (~create_l1) then begin
      mg_log, 'skipping L1 processing', name='comp', /info
    endif

    if (distribute_l1) then begin
      mg_log, 'distributing L1 data', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (~dry_run) then comp_distribute_l1, date_dir, process_wavelengths[w]
      endfor
      ; distribute files not associated with a wavelength such as flats/darks
      if (~dry_run) then comp_distribute_l1, date_dir
    endif else begin
      mg_log, 'skipping L1 distribution', name='comp', /info
    endelse

    ;---------------  Level_2 data processing  ---------------

    if (create_l2 || create_average || create_quick_invert || find_systematics $
          || create_analysis || create_daily_images || create_movies $
          || create_daily_summaries) then begin
      mg_log, 'starting level 2 processing for %s', date_dir, name='comp', /info

      if (~dry_run) then begin
        process_dir = filepath(date_dir, root=process_basedir)
        if (~file_test(process_dir, /directory)) then file_mkdir, process_dir

        l2_process_dir = filepath('level2', root=process_dir)
        if (~file_test(l2_process_dir, /directory)) then file_mkdir, l2_process_dir

        process_config_filename = filepath('comp.cfg', root=l2_process_dir)
        file_copy, _config_filename, process_config_filename, /overwrite
      endif
    endif

    if (create_average) then begin
      ; compute the mean, median and standard deviation of L1 data
      mg_log, 'creating averages', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (process_wavelengths[w] ne '1083') then begin
          if (~dry_run) then begin
            comp_average, date_dir, process_wavelengths[w], error=error
            if (error ne 0) then continue
          endif
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif else begin
      mg_log, 'skipping create average', name='comp', /info
    endelse

    if (create_quick_invert) then begin
      ; perform 'quick' inversion
      mg_log, 'creating quick invert', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (process_wavelengths[w] ne '1083') then begin
          if (~dry_run) then begin
            comp_quick_invert, date_dir, process_wavelengths[w], error=error
            if (error ne 0) then continue
          endif
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif else begin
      mg_log, 'skipping quick invert', name='comp', /info
    endelse

    if (find_systematics) then begin
      ; evaluate systematic errors in comp data
      mg_log, 'finding systematics', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (process_wavelengths[w] ne '1083') then begin
          if (~dry_run) then begin
            comp_find_systematics, date_dir, process_wavelengths[w], 'mean', error=error
            if (error ne 0) then continue
          endif
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif else begin
      mg_log, 'skipping find systematics', name='comp', /info
    endelse

    if (create_analysis) then begin
      mg_log, 'running 3-point analysis', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (process_wavelengths[w] ne '1083') then begin
          if (~dry_run) then begin
            comp_l2_analytical, date_dir, process_wavelengths[w], nwl=3
          endif
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif else begin
      mg_log, 'skipping creating analysis', name='comp', /info
    endelse

    if (create_daily_images) then begin
      mg_log, 'creating daily images', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (process_wavelengths[w] ne '1083') then begin
          if (~dry_run) then begin
            comp_l2_write_daily_images, date_dir, process_wavelengths[w], $
                                        n_avrg=50
          endif
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif else begin
      mg_log, 'skipping creating daily images', name='comp', /info
    endelse

    if (create_movies) then begin
      mg_log, 'creating movies', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (process_wavelengths[w] ne '1083') then begin
          if (~dry_run) then begin
            comp_l2_create_movies, date_dir, process_wavelengths[w], nwl=3
          endif
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif else begin
      mg_log, 'skipping creating movies', name='comp', /info
    endelse

    if (create_daily_summaries) then begin
      mg_log, 'making daily summaries', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (~dry_run) then begin
          comp_l2_summary,  date_dir, process_wavelengths[w]
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif else begin
      mg_log, 'skipping creating daily summaries', name='comp', /info
    endelse

    if (distribute_l2) then begin
      mg_log, 'distributing L2 data', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (~dry_run) then begin
          comp_distribute_l2, date_dir, process_wavelengths[w]
        endif
      endfor
    endif else begin
      mg_log, 'skipping L2 distribution', name='comp', /info
    endelse

    if (update_database) then begin
      mg_log, 'updating database', name='comp', /info
      db_t0 = systime(/seconds)
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (~dry_run) then begin
          comp_update_database, date_dir, process_wavelengths[w]
        endif
      endfor
      db_t1 = systime(/seconds)
      mg_log, 'Total time for COMP_UPDATE_DATABASE: %0.1f seconds', $
              db_t1 - db_t0, $
              name='comp', /debug
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif else begin
      mg_log, 'skipping updating database', name='comp', /info
    endelse

    t1 = systime(/seconds)
    mg_log, 'Total running time: %0.2f sec', t1 - t0, name='comp', /info

    if (lock_raw && ~dry_run) then begin
      unlocked = comp_state(date_dir, /unlock)
      mg_log, 'Unlocked %s', filepath(date_dir, root=raw_basedir), $
              name='comp', /info
      processed = comp_state(date_dir, /processed)
      mg_log, 'Marked %s as processed', filepath(date_dir, root=raw_basedir), $
              name='comp', /info
    endif
  endfor

  !except = orig_except

  mg_log, /quit
end
