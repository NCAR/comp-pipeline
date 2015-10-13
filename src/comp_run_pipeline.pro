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
      unlocked = comp_state(date_dir, /unlock)
      if (unlocked) then begin
        mg_log, 'Unlocked %s', filepath(date_dir, root=raw_basedir), $
                name='comp', /info
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
    mg_log, 'no days to process found in raw directory', name='comp', /error
    mg_log, 'Total running time: %0.2f sec', t1 - t0, name='comp', /info
    mg_log, /quit
    return
  endif

  dirs = candidate_dirs[date_dirs_ind]

  for d = 0L, n_dirs - 1L do begin
    t0 = systime(/seconds)

    date_dir = dirs[d]

    available = comp_state(date_dir, /lock)
    if (available ne 1) then begin
      continue
    endif

    comp_initialize, date_dir
    comp_setup_loggers_date, date_dir

    mg_log, 'Locked %s', filepath(date_dir, root=raw_basedir), $
            name='comp', /info
    mg_log, 'starting processing for %d', date_dir, name='comp', /info

    code_version = comp_find_code_version(revision=revision)
    code_revision = revision

    ;---------------  Prep  ----------------------------------------

    if (validate) then begin
      valid = comp_validator(date_dir)
      if (~valid) then begin
        mg_log, 'skipping %s...', date_dir, name='comp', /info
        unlocked = comp_state(date_dir, /unlock)
        mg_log, 'Unlocked %s', filepath(date_dir, root=raw_basedir), $
                name='comp', /info
        continue
      endif
    endif

    ;---------------  Level_1 data processing  ---------------------

    mg_log, 'starting processing for %s', date_dir, name='comp', /info
    mg_log, 'memory usage: %0.1fM', $
            (memory(/highwater) - start_memory) / 1024. / 1024., $
            name='comp', /debug

    ; copy configuration file to the process output directory
    process_dir = filepath(date_dir, root=process_basedir)
    file_mkdir, process_dir
    file_copy, _config_filename, process_dir, /overwrite

    mg_log, 'running file_type', name='comp', /info
    file_type_t0 = systime(/seconds)
    ; take inventory of the data for this day
    comp_file_type, date_dir
    file_type_t1 = systime(/seconds)
    mg_log, 'Total time for COMP_FILE_TYPE: %0.1f seconds', $
            file_type_t1 - file_type_t0, $
            name='comp', /debug
    mg_log, 'memory usage: %0.1fM', $
            (memory(/highwater) - start_memory) / 1024. / 1024., $
            name='comp', /debug

    mg_log, 'running dark mode', name='comp', /info

    ; reduce bias images for this day
    comp_make_dark, date_dir, error=error
    if (error ne 0) then begin
      unlocked = comp_state(date_dir, /unlock)
      mg_log, 'Unlocked %s', filepath(date_dir, root=raw_basedir), $
              name='comp', /info
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
      unlocked = comp_state(date_dir, /unlock)
      mg_log, 'Unlocked %s', filepath(date_dir, root=raw_basedir), $
              name='comp', /info
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

    mg_log, 'running comp_extract_intensity', name='comp', /info
    ; extract intensity images from Level_1 files
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

    mg_log, 'running comp_gbu', name='comp', /info
    ; identify good data
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

    ;---------------  Level_2 data processing  ---------------

    ; mg_log, 'running comp_average', name='comp', /info
    ; ; compute the mean, median and standard deviation of Level_1 data
    ; for w = 0L, n_elements(process_wavelengths) - 1L do begin
    ;   if (process_wavelengths[w] ne '1083') then begin
    ;     comp_average, date_dir, process_wavelengths[w], error=error
    ;     if (error ne 0) then continue
    ;   endif
    ; endfor
    ; mg_log, 'memory usage: %0.1fM', $
    ;         (memory(/highwater) - start_memory) / 1024. / 1024., $
    ;         name='comp', /debug
    ;
    ; mg_log, 'running comp_quick_invert', name='comp', /info
    ; ; perform 'quick' inversion
    ; for w = 0L, n_elements(process_wavelengths) - 1L do begin
    ;   if (process_wavelengths[w] ne '1083') then begin
    ;     comp_quick_invert, date_dir, process_wavelengths[w], error=error
    ;     if (error ne 0) then continue
    ;   endif
    ; endfor
    ; mg_log, 'memory usage: %0.1fM', $
    ;         (memory(/highwater) - start_memory) / 1024. / 1024., $
    ;         name='comp', /debug
    ;
    ; mg_log, 'running comp_find_systematics', name='comp', /info
    ; ; evaluate systematic errors in comp data
    ; for w = 0L, n_elements(process_wavelengths) - 1L do begin
    ;   if (process_wavelengths[w] ne '1083') then begin
    ;     comp_find_systematics, date_dir, process_wavelengths[w], 'mean', error=error
    ;     if (error ne 0) then continue
    ;   endif
    ; endfor
    ; mg_log, 'memory usage: %0.1fM', $
    ;         (memory(/highwater) - start_memory) / 1024. / 1024., $
    ;         name='comp', /debug
    ;
    ;
    ; ;---------------  Level LC data processing  ---------------
    ;
    ; mg_log, 'running 3-points analysis', name='comp', /info
    ; for w = 0L, n_elements(process_wavelengths) - 1L do begin
    ;   if (process_wavelengths[w] ne '1083') then begin
    ;     comp_l2_analytical_three, date_dir, process_wavelengths[w]
    ;   endif
    ; endfor
    ; mg_log, 'memory usage: %0.1fM', $
    ;         (memory(/highwater) - start_memory) / 1024. / 1024., $
    ;         name='comp', /debug
    ;
    ; for w = 0L, n_elements(process_wavelengths) - 1L do begin
    ;   if (process_wavelengths[w] ne '1083') then begin
    ;     comp_l2_create_jpgs, date_dir, process_wavelengths[w], nwl=3, /seq
    ;   endif
    ; endfor
    ; mg_log, 'memory usage: %0.1fM', $
    ;         (memory(/highwater) - start_memory) / 1024. / 1024., $
    ;         name='comp', /debug
    ;
    ; for w = 0L, n_elements(process_wavelengths) - 1L do begin
    ;   if (process_wavelengths[w] ne '1083') then begin
    ;     comp_l2_create_movies, date_dir, process_wavelengths[w], nwl=3
    ;   endif
    ; endfor
    ; mg_log, 'memory usage: %0.1fM', $
    ;         (memory(/highwater) - start_memory) / 1024. / 1024., $
    ;         name='comp', /debug
    ;
    ; ;comp_quickview, date_dir, '1074', nwl=3,  dynamics=0
    ;
    ; ;comp_l2_analytical_three, date_dir, '1079'
    ; ;comp_l2_create_jpgs, date_dir, '1079', nwl=3, /seq
    ; ;comp_l2_create_movies, date_dir, '1079', nwl=3
    ;
    ; mg_log, 'running 5-points analysis', name='comp', /info
    ; for w = 0L, n_elements(process_wavelengths) - 1L do begin
    ;   if (process_wavelengths[w] ne '1083') then begin
    ;     comp_l2_analytical_five, date_dir , process_wavelengths[w]
    ;   endif
    ; endfor
    ; mg_log, 'memory usage: %0.1fM', $
    ;         (memory(/highwater) - start_memory) / 1024. / 1024., $
    ;         name='comp', /debug
    ;
    ; for w = 0L, n_elements(process_wavelengths) - 1L do begin
    ;   if (process_wavelengths[w] ne '1083') then begin
    ;     comp_l2_create_jpgs, date_dir, process_wavelengths[w], nwl=5, /seq
    ;   endif
    ; endfor
    ; mg_log, 'memory usage: %0.1fM', $
    ;         (memory(/highwater) - start_memory) / 1024. / 1024., $
    ;         name='comp', /debug
    ;
    ; for w = 0L, n_elements(process_wavelengths) - 1L do begin
    ;   if (process_wavelengths[w] ne '1083') then begin
    ;     comp_l2_create_movies, date_dir, process_wavelengths[w], nwl=5
    ;   endif
    ; endfor
    ; mg_log, 'memory usage: %0.1fM', $
    ;         (memory(/highwater) - start_memory) / 1024. / 1024., $
    ;         name='comp', /debug
    ;
    ; ;comp_l2_analytical_five, date_dir , '1079'
    ; ;comp_l2_create_jpgs, date_dir, '1079', nwl=5, /seq
    ; ;comp_l2_create_movies, date_dir, '1079' , nwl=5
    ;
    ; mg_log, 'making tarballs', name='comp', /info
    ; for w = 0L, n_elements(process_wavelengths) - 1L do begin
    ;   comp_infofile_and_tarballs,  date_dir, process_wavelengths[w]
    ; endfor
    ; mg_log, 'memory usage: %0.1fM', $
    ;         (memory(/highwater) - start_memory) / 1024. / 1024., $
    ;         name='comp', /debug
    ; ;comp_infofile_and_tarballs,  date_dir, '1079'

    t1 = systime(/seconds)
    mg_log, 'Total running time: %0.2f sec', t1 - t0, name='comp', /info

    unlocked = comp_state(date_dir, /unlock)
    mg_log, 'Unlocked %s', filepath(date_dir, root=raw_basedir), $
            name='comp', /info
  endfor

  mg_log, /quit
end
