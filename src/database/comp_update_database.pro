; docformat = 'rst'

;+
; Update database with results of pipeline run for the given day.
;
; :Params:
;   date : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;-
pro comp_update_database, date, wave_type
  compile_opt strictarr
  @comp_config_common
  
  ; create MySQL database interface object
  db = mgdbmysql()
  db->connect, config_filename=database_config_filename, $
               config_section=database_config_section, $
               status=status, error_message=error_message
  if (status eq 0L) then begin
    db->getProperty, host_name=host
    mg_log, 'connected to %s', host, name='comp', /info
  endif else begin
    mg_log, 'failed to connect to database', name='comp', /error
    mg_log, '%s', error_message, name='comp', /error
    return
  endelse

  obs_day = strmid(date, 0, 4) + '-' + strmid(date, 4, 2) + '-' + strmid(date, 6, 2)
  obs_day_index = 0
  
  ; check to see if passed observation day date is in mlso_numfiles table
  obs_day_results = db->query('SELECT count(obs_day) FROM mlso_numfiles WHERE obs_day=''%s''', $
                              obs_day, fields=fields)
  obs_day_count = obs_day_results.count_obs_day_

  if (obs_day_count eq 0) then begin
    ; if not already in table, create a new entry for the passed observation day
    db->execute, 'INSERT INTO mlso_numfiles (obs_day) VALUES (''%s'') ', $
                 obs_day, $
                 status=status, error_message=error_message, $
                 sql_statement=sql_cmd, $
                 n_warnings=n_warnings
    if (status ne 0L) then begin
      mg_log, 'error inserting into mlso_numfiles table', name='comp', /error
      mg_log, 'status: %d, error message: %s', status, error_message, $
              name='comp', /error
      mg_log, 'SQL command: %s', sql_cmd, name='comp', /error
    endif

    if (n_warnings gt 0L) then comp_db_log_warnings, database=db

    obs_day_index = db->query('SELECT LAST_INSERT_ID()')
  endif else begin
    ; if it is in the database, get the corresponding index, day_id
    obs_day_results = db->query('SELECT day_id FROM mlso_numfiles WHERE obs_day=''%s''', $
                                obs_day, fields=fields)
    obs_day_index = obs_day_results.day_id
  endelse

  ; comp_sw_insert
  ; comp_file_insert
  ; comp_img_insert
  ; comp_eng_insert
  ; comp_cal_insert

  ; comp_sci_insert, date, wave_type, database=db, obsday_index=obsday_index

  ; close database connection
  obj_destroy, db
end
