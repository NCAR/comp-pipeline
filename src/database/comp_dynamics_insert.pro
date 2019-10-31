; docformat = 'rst'

;+
; Update comp_dynamics table with results of pipeline run for the given day.
;
; :Params:
;   date : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :Keywords:
;   database : in, required, type=MGdbMySQL object
;     database connection
;   obsday_index : in, required, type=integer
;     index into mlso_numfiles database table
;
; :Author:
;   MLSO Software Team
;-
pro comp_dynamics_insert, date, wave_type, $
                          database=db, obsday_index=obsday_index
  compile_opt strictarr

  basename = string(date, wave_type, format='(%"%s.comp.%s.dynamics.fts*")')
  dynamics_files = file_search(filepath(basename, $
                                        subdir=[date, 'level2'], $
                                        root=process_basedir), $
                               count=n_dynamics_files)

  if (n_dynamics_files gt 0L) then begin
    mg_log, 'inserting %d rows into %s nm comp_dynamics table...', $
            n_dynamics_files, wave_type, $
            name='comp', /info
  endif else begin
    mg_log, 'no dynamics files to insert into %s nm comp_dynamics table', $
            wave_type, $
            name='comp', /info
    goto, done
  endelse

  ; loop through dynamics files
  for f = 0L, n_dynamics_files - 1L do begin
    fits_open, dynamics_files[f], fcb
    fits_read, fcb, data, primary_header, exten_no=0, /no_abort, message=msg
    fits_read, fcb, intensity, intensity_header, exten_no=1, /no_abort, message=msg
    fits_read, fcb, velocity, velocity_header, exten_no=3, /no_abort, message=msg
    fits_close, fcb
    if (msg ne '') then message, msg

    date_obs = string(sxpar(primary_header, 'DATE-OBS'), $
                     sxpar(primary_header, 'TIME-OBS'), $
                     format='(%"%sT%s")')
    date_obs = comp_normalize_datetime(date_obs)

    intensity_max = max(intensity)
    doppler_min = min(velocity, max=doppler_max)

    ; insert into comp_dynamics table
    fields = [{name: 'file_name', type: '''%s'''}, $
              {name: 'date_obs', type: '''%s'''}, $
              {name: 'obs_day', type: '%d'}, $

              {name: 'intensity_max', type: '%f'}, $
              {name: 'doppler_min', type: '%f'}, $
              {name: 'doppler_max', type: '%f'}]
    sql_cmd = string(strjoin(fields.name, ', '), $
                     strjoin(fields.type, ', '), $
                     format='(%"insert into comp_dynamics (%s) values (%s)")')
    db->execute, sql_cmd, $
                 file_basename(dynamics_files[f], '.gz'), $
                 date_obs, $
                 obsday_index, $

                 intensity_max, $
                 doppler_min, $
                 doppler_max, $

                 status=status, $
                 error_message=error_message, $
                 sql_statement=final_sql_cmd, $
                 n_warnings=n_warnings
    if (status ne 0L) then begin
      mg_log, 'error inserting %s into comp_dynamics table', $
              file_basename(dynamics_files[f]), $
              name='comp', /error
      mg_log, 'status: %d, error message: %s', status, error_message, $
              name='comp', /error
      mg_log, 'SQL command: %s', final_sql_cmd, name='comp', /error
    endif

    if (n_warnings gt 0L) then comp_db_log_warnings, database=db
  endfor

  done:
  mg_log, 'done', name='comp', /info
end
