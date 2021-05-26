; docformat = 'rst'

;+
; Procedure to run all the routines in the CoMP pipeline.
;
; :Author:
;   MLSO Software Team
;
; :Keywords:
;   config_filename, in, optional, type=string
;     configuration filename to use, default is `comp.cfg` in the `src`
;     directory
;-
pro comp_run_pipeline, config_filename=config_filename
  compile_opt strictarr
  @comp_config_common
  @comp_diagnostics_common

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
    mg_log, 'Total running time: %0.2f sec', $
            mg_secs2hms(t1 - t0, format='%d hr %d min %4.1f sec'), $
            name='comp', /info

    if (n_elements(date_dir) gt 0) then begin
      if (lock_raw) then begin
        unlocked = comp_state(date_dir, /unlock)
        if (unlocked) then begin
          mg_log, 'Unlocked %s', filepath(date_dir, root=raw_basedir), $
                  name='comp', /info
        endif
      endif
    endif

    comp_crash_notification, date_dir

    mg_log, /quit
    return
  endif

  loop_time = 0.

  start_memory = memory(/current)

  comp_configuration, config_filename=_config_filename, error=error
  if (error) then begin
    mg_log, 'error in configuration, exiting', name='comp', /critical
    goto, done
  endif

  comp_setup_loggers

  candidate_dates = comp_expand_date_expr(date_pattern, count=n_candidate_dates)
  if (n_candidate_dates eq 0) then begin
    t1 = systime(/seconds)
    mg_log, 'no days to process', name='comp', /error
    mg_log, 'total running time: %0.2f sec', t1 - t0, name='comp', /info
    mg_log, /quit
    return
  endif

  ; ignore math errors
  orig_except = !except
  !except = 0

  for d = 0L, n_candidate_dates - 1L do begin
    t0 = systime(/seconds)

    error = 0L
    success_with_day = 0B
    date_dir = candidate_dates[d]

    comp_update_configuration, date_dir
    comp_initialize, date_dir
    if (~dry_run) then comp_setup_loggers_date, date_dir, rotate=rotate_logs

    if (n_elements(raw_basedir) eq 0L) then begin
      mg_log, 'no raw basedir specified for %s, skipping', date_dir, $
              name='comp', /error
      continue
    endif

    if (~file_test(filepath(date_dir, root=raw_basedir), /directory)) then begin
      mg_log, 'no raw base directory for %s, skipping', date_dir, $
              name='comp', /error
      continue
    endif

    if (lock_raw) then begin
      available = comp_state(date_dir, n_concurrent=n_concurrent)
      if (available ne 1) then begin
        mg_log, '%s locked, skipping...', date_dir, name='comp', /info
        continue
      endif else begin
        if (~dry_run) then begin
          if (n_concurrent gt max_n_concurrent) then begin
            mg_log, '%d processes running currently, quitting', n_concurrent, $
                    name='comp', /info
            goto, done
          endif
          available = comp_state(date_dir, /lock, n_concurrent=n_concurrent)
          mg_log, 'locked %s', filepath(date_dir, root=raw_basedir), $
                  name='comp', /info
        endif
      endelse
    endif

    ; if (~dry_run && reprocess && rotate_logs) then begin
    ;   comp_setup_loggers_date, date_dir, /rotate
    ; endif

    mg_log, 'starting processing for %d', date_dir, name='comp', /info

    version = comp_find_code_version(revision=revision, branch=branch)
    mg_log, 'comp-pipeline %s (%s on %s)', version, revision, branch, $
            name='comp', /info
    mg_log, 'using IDL %s on %s', !version.release, !version.os_name, $
            name='comp', /info

    if (~dry_run) then comp_setup_loggers_eng, date_dir

    ;---------------  Prep  ----------------------------------------

    ; prepare if reprocessing
    if (~dry_run && reprocess) then begin
      mg_log, 'prepping for reprocessing', name='comp', /info
      comp_reprocess, date_dir
    endif

    if (validate) then begin
      mg_log, 'validating raw data', name='comp', /info
      if (~dry_run) then begin
        valid = comp_validator(date_dir)
        if (~valid) then begin
          mg_log, 'skipping %s...', date_dir, name='comp', /info
          if (lock_raw) then begin
            unlocked = comp_state(date_dir, /unlock, n_concurrent=n_concurrent)
            mg_log, 'unlocked %s', filepath(date_dir, root=raw_basedir), $
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

      process_config_filename = filepath(string(date_dir, format='(%"%s.comp.l1.cfg")'), $
                                         root=l1_process_dir)
      file_copy, _config_filename, process_config_filename, /overwrite

      ; write constants used for given day to engineering directory
      comp_write_epochs, date_dir

      ; take inventory of the data for this day
      mg_log, 'determining file types', name='comp', /info
      file_type_t0 = systime(/seconds)
      if (~dry_run) then comp_file_type, date_dir
      file_type_t1 = systime(/seconds)
      mg_log, 'total time for COMP_FILE_TYPE: %0.1f seconds', $
              file_type_t1 - file_type_t0, $
              name='comp', /debug
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif

    if (create_flatsdarks) then begin
      ; reduce bias images for this day
      error = 0L
      mg_log, 'making darks', name='comp', /info
      if (~dry_run) then comp_make_dark, date_dir, error=error

      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      if (error ne 0) then begin
        if (lock_raw) then begin
          unlocked = comp_state(date_dir, /unlock, n_concurrent=n_concurrent)
          mg_log, 'unlocked %s', filepath(date_dir, root=raw_basedir), $
                  name='comp', /info
          processed = comp_state(date_dir, /processed, n_concurrent=n_concurrent)
          mg_log, 'marked %s as processed', filepath(date_dir, root=raw_basedir), $
                  name='comp', /info
        endif
        mg_log, 'error processing darks, stopping day', name='comp', /error
        goto, done_with_day
      endif

      mg_log, 'making flats', name='comp', /info
      make_flat_t0 = systime(/seconds)
      ; reduce opal images for this day
      error = 0L
      if (~dry_run) then begin
        comp_make_flat, date_dir, error=error
        if (correct_continuum && error eq 0) then begin
          comp_continuum_correction, date_dir
        endif else begin
          mg_log, 'skipping continuum correction', name='comp', /info
        endelse
      endif
      make_flat_t1 = systime(/seconds)
      mg_log, 'total time for COMP_MAKE_FLAT: %0.1f seconds', $
              make_flat_t1 - make_flat_t0, $
              name='comp', /debug
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      if (error ne 0) then begin
        if (lock_raw) then begin
          unlocked = comp_state(date_dir, /unlock, n_concurrent=n_concurrent)
          mg_log, 'unlocked %s', filepath(date_dir, root=raw_basedir), $
                  name='comp', /info
          processed = comp_state(date_dir, /processed, n_concurrent=n_concurrent)
          mg_log, 'marked %s as processed', filepath(date_dir, root=raw_basedir), $
                  name='comp', /info
        endif
        mg_log, 'error processing flats, stopping day', name='comp', /error
        goto, done_with_day
      endif
    endif

    if (create_l1) then begin
      mg_log, 'running L1 processing', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        l1_process_t0 = systime(/seconds)
        error = 0L
        if (~dry_run) then begin
          comp_l1_process, date_dir, process_wavelengths[w], error=error
        endif
        l1_process_t1 = systime(/seconds)
        mg_log, 'total time for COMP_L1_PROCESS: %0.1f seconds', $
                l1_process_t1 - l1_process_t0, $
                name='comp', /debug
        if (error ne 0L) then begin
          mg_log, 'error with L1 processing, stopping day', name='comp', /error
          goto, done_with_day
        endif
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
      mg_log, 'total time for COMP_UPDATE_FILENAMES: %0.1f seconds', $
              update_filenames_t1 - update_filenames_t0, $
              name='comp', /debug
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug

      ; extract intensity images from Level_1 files
      mg_log, 'extracting intensity', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        extract_intensity_t0 = systime(/seconds)
        error = 0L
        if (~dry_run) then begin
          comp_extract_intensity, date_dir, process_wavelengths[w], error=error
          if (process_wavelengths[w] eq '1074') then begin
            comp_extract_intensity, date_dir, process_wavelengths[w], $
                                    /background, $
                                    error=error
          endif
        endif
        extract_intensity_t1 = systime(/seconds)
        mg_log, 'total time for COMP_EXTRACT_INTENSITY: %0.1f seconds', $
                extract_intensity_t1 - extract_intensity_t0, $
                name='comp', /debug
        if (error ne 0) then begin
          mg_log, 'error with extracting intensity, stopping day', name='comp', /error
          goto, done_with_day
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif

    ; identify good data
    if (create_l1 || perform_gbu) then begin
      mg_log, 'determining GBU', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        gbu_t0 = systime(/seconds)
        if (~dry_run) then begin
          comp_gbu, date_dir, process_wavelengths[w], error=error
        endif
        gbu_t1 = systime(/seconds)
        mg_log, 'total time for COMP_GBU: %0.1f seconds', $
                gbu_t1 - gbu_t0, $
                name='comp', /debug
        if (error ne 0) then begin
          mg_log, 'error with determing GBU, stopping day', name='comp', /error
          goto, done_with_day
        endif

        gbu_plot_filename = filepath(string(date_dir, process_wavelengths[w], $
                                            format='(%"%s.comp.%s.gbu.png")'), $
                                     subdir=comp_decompose_date(date_dir), $
                                     root=engineering_dir)
        gbu_filename = filepath(string(date_dir, process_wavelengths[w], $
                                       format='(%"%s.comp.%s.gbu.log")'), $
                                subdir=[date_dir, 'level1'], $
                                root=process_basedir)
        if (file_test(gbu_filename, /regular)) then begin
          comp_plot_gbu, date_dir, process_wavelengths[w], $
                         gbu_plot_filename, gbu_filename
        endif else begin
          mg_log, 'skipping GBU plot for %s nm...', process_wavelengths[w], $
                  name='comp', /warn
        endelse
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif else begin
      mg_log, 'skipping GBU', name='comp', /info
    endelse

    if (create_l1 || perform_gbu) then begin
      mg_log, 'creating mp4 of bad science images', name='comp', /info
      comp_create_bad_mp4, date_dir
    endif else begin
      mg_log, 'skipping creating mp4 of bad science images', name='comp', /info
    endelse


    if (create_l1 || perform_gbu) then begin
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        mg_log, 'plotting daily %s engineering data', process_wavelengths[w], $
                name='comp', /info
        plot_eng_t0 = systime(/seconds)
        if (~dry_run) then begin
          comp_plot_engineering, date_dir, process_wavelengths[w]
        endif
        plot_eng_t1 = systime(/seconds)
        mg_log, 'total time for COMP_PLOT_ENGINEERING: %0.1f seconds', $
                plot_eng_t1 - plot_eng_t0, $
                name='comp', /debug
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif else begin
      mg_log, 'skipping L1 processing', name='comp', /info
    endelse

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

        process_config_filename = filepath(string(date_dir, format='(%"%s.comp.l2.cfg")'), $
                                           root=l2_process_dir)
        file_copy, _config_filename, process_config_filename, /overwrite
      endif
    endif

    if (create_average) then begin
      ; compute the mean, median and standard deviation of L1 data
      mg_log, 'creating averages', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (process_wavelengths[w] ne '1083') then begin
          if (~dry_run) then begin
            comp_average, date_dir, process_wavelengths[w], error=error, $
                          found_files=waves_files_found
            if (error ne 0) then begin
              mg_log, 'error with creating wave averages, stopping day', $
                      name='comp', /error
              goto, done_with_day
            endif

            comp_average, date_dir, process_wavelengths[w], /synoptic, error=error, $
                          found_files=synoptic_files_found
            if (error ne 0) then begin
              mg_log, 'error with creating synoptic averages, stopping day', $
                      name='comp', /error
              goto, done_with_day
            endif
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
            comp_quick_invert, date_dir, process_wavelengths[w], $
                               method='median', error=error
            if (error ne 0) then begin
              mg_log, 'error with creating wave quick invert, stopping day', $
                      name='comp', /error
              goto, done_with_day
            endif

            comp_quick_invert, date_dir, process_wavelengths[w], $
                               method='mean', error=error
            if (error ne 0) then begin
              mg_log, 'error with creating wave quick invert, stopping day', $
                      name='comp', /error
              goto, done_with_day
            endif

            comp_quick_invert, date_dir, process_wavelengths[w], $
                               /synoptic, method='median', error=error
            if (error ne 0) then begin
              mg_log, 'error with creating synoptic quick invert, stopping day', $
                      name='comp', /error
              goto, done_with_day
            endif

            comp_quick_invert, date_dir, process_wavelengths[w], $
                               /synoptic, method='mean', error=error
            if (error ne 0) then begin
              mg_log, 'error with creating synoptic quick invert, stopping day', $
                      name='comp', /error
              goto, done_with_day
            endif
          endif
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif else begin
      mg_log, 'skipping quick invert', name='comp', /info
    endelse

    if (create_full_invert) then begin
      ; perform 'full' inversion
      mg_log, 'creating full invert', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (process_wavelengths[w] ne '1083') then begin
          if (~dry_run) then begin
            comp_analyze, date_dir, process_wavelengths[w], error=error
            if (error ne 0) then begin
              mg_log, 'error with creating full invert, stopping day', name='comp', /error
              goto, done_with_day
            endif
          endif
        endif
      endfor
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif else begin
      mg_log, 'skipping full invert', name='comp', /info
    endelse

    if (find_systematics) then begin
      ; evaluate systematic errors in comp data
      mg_log, 'finding systematics', name='comp', /info
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (process_wavelengths[w] ne '1083') then begin
          if (~dry_run) then begin
            comp_find_systematics, date_dir, process_wavelengths[w], 'mean', error=error
            if (error ne 0) then begin
              mg_log, 'error with finding systematics, stopping day', name='comp', /error
              goto, done_with_day
            endif
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
            comp_l2_write_daily_images, date_dir, process_wavelengths[w]
            comp_l2_write_daily_images, date_dir, process_wavelengths[w], $
                                        /median, /waves
            comp_l2_write_daily_images, date_dir, process_wavelengths[w], $
                                        /median, /synoptic
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

      obsday_index = mlso_obsday_insert(date_dir, $
                                        database_config_filename, $
                                        database_config_section, $
                                        database=db, $
                                        status=status, $
                                        log_name='comp')
      comp_db_clearday, database=db, obsday_index=obsday_index

      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (~dry_run) then begin
          comp_update_database, date_dir, process_wavelengths[w], $
                                database=db, obsday_index=obsday_index
        endif
      endfor
      if (~dry_run) then begin
        comp_cal_insert, date_dir, database=db, obsday_index=obsday_index
        comp_numfiles_update, date_dir, database=db, obsday_index=obsday_index
      endif
      obj_destroy, db

      db_t1 = systime(/seconds)
      mg_log, 'total time for COMP_UPDATE_DATABASE: %0.1f seconds', $
              db_t1 - db_t0, $
              name='comp', /debug
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif else begin
      mg_log, 'skipping updating database', name='comp', /info
    endelse

    mg_log, 'checking for leaked LUNs...', name='comp', /info
    help, /files, output=output
    for i = 1L, n_elements(output) - 1L do begin
      filename = (strsplit(output[i], /extract))[-1]
      mg_log, 'leaked LUN for %s', filename, name='comp', /error
    endfor

    if (check_l1) then begin
      check_l1_t0 = systime(/seconds)

      ; check metrics of L1 data

      comp_l1_check_all, date_dir, body=body
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        mg_log, 'checking %s L1 data', process_wavelengths[w], $
                name='comp', /info
        if (~dry_run) then begin
          comp_l1_check, date_dir, process_wavelengths[w], body=body
        endif
      endfor

      comp_send_notification, date_dir, body, t0, gbu_plot_filename, config_filename

      comp_l1_check_all, date_dir, body=summary_body, /no_log_message
      for w = 0L, n_elements(process_wavelengths) - 1L do begin
        if (~dry_run) then begin
          comp_l1_check, date_dir, process_wavelengths[w], body=summary_body
        endif
      endfor

      summary_filename = filepath(string(date_dir, format='(%"%s.comp.l1.summary.txt")'), $
                                  subdir=[date_dir, 'level1'], $
                                  root=process_basedir)
      openw, lun, summary_filename, /get_lun
      printf, lun, transpose(summary_body->toArray())
      free_lun, lun

      if (obj_valid(body)) then obj_destroy, body
      if (obj_valid(summary_body)) then obj_destroy, summary_body

      check_l1_t1 = systime(/seconds)
      mg_log, 'total time for COMP_L1_CHECK: %0.1f seconds', $
              check_l1_t1 - check_l1_t0, $
              name='comp', /debug
      mg_log, 'memory usage: %0.1fM', $
              (memory(/highwater) - start_memory) / 1024. / 1024., $
              name='comp', /debug
    endif

    success_with_day = 1B

    done_with_day:
    t1 = systime(/seconds)
    mg_log, 'total running time: %s', comp_sec2str(t1 - t0), $
            name='comp', /info

    if (lock_raw && ~dry_run) then begin
      unlocked = comp_state(date_dir, /unlock, n_concurrent=n_concurrent)
      mg_log, 'unlocked %s', filepath(date_dir, root=raw_basedir), $
              name='comp', /info
      if (success_with_day) then begin
        processed = comp_state(date_dir, /processed, n_concurrent=n_concurrent)
        mg_log, 'marked %s as processed', filepath(date_dir, root=raw_basedir), $
                name='comp', /info
      endif
    endif
  endfor

  done:
  !except = orig_except
  mg_log, /quit
end
