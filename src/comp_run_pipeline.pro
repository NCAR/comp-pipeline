; docformat = 'rst'

;+
; Run the pipeline.
;
; :Params:
;   configuration_filename : in, required, type=string
;     filename of configuration file specifying the parameters of the run
;-
pro comp_run_pipeline, configuration_filename
  compile_opt strictarr

  t0 = systime(/seconds)
  catch, error
  if (error ne 0) then begin
    catch, /cancel
    t1 = systime(/seconds)
    mg_log, /last_error, name='comp', /critical
    mg_log, 'Total running time: %0.2f sec', t1 - t0, name='comp', /info

    mg_log, /quit
    return
  endif

  config = comp_configuration(filename=configuration_filename, /use_environment)

  search_pattern = filepath(config->get('input', 'date_pattern'), $
                            root=config->get('input', 'raw_basedir'))

  candidate_dirs = file_search(search_pattern, $
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
    mg_log, 'Starting to process %s', dirs[d], name='comp', /info

    ; prep/validation
    ; level 1 processing
    ; level 2 processing
  endfor

  obj_destroy, config
end
