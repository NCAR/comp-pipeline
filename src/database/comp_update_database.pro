; docformat = 'rst'

;+
; Update database with results of pipeline run for the given day.
;
; :Params:
;   date : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :Author:
;   MLSO Software Team
;-
pro comp_update_database, date, wave_type, $
                          database=db, obsday_index=obsday_index
  compile_opt strictarr
  @comp_config_common
  
  ; create MySQL database interface object
  if (~obj_valid(db)) then begin
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
  endif

  obs_day = strmid(date, 0, 4) + '-' + strmid(date, 4, 2) + '-' + strmid(date, 6, 2)
  obs_day_index = 0
  
  ; check to see if passed observation day date is in mlso_numfiles table
  query = 'select count(obs_day) from mlso_numfiles where obs_day=''%s'''
  obs_day_results = db->query(query, obs_day, fields=fields)
  obs_day_count = obs_day_results.count_obs_day_

  if (obs_day_count eq 0) then begin
    ; if not already in table, create a new entry for the passed observation day
    db->execute, 'insert into mlso_numfiles (obs_day) values (''%s'') ', $
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

    obs_day_index = db->query('select last_insert_id()')
  endif else begin
    ; if it is in the database, get the corresponding index, day_id
    query = 'select day_id from mlso_numfiles where obs_day=''%s'''
    obs_day_results = db->query(query, obs_day, fields=fields)
    obs_day_index = obs_day_results.day_id
  endelse

  comp_file_insert, date, wave_type, database=db, obsday_index=obsday_index
  comp_eng_insert, date, wave_type, database=db, obsday_index=obsday_index
  comp_sci_insert, date, wave_type, database=db, obsday_index=obsday_index
  comp_dynamics_insert, date, wave_type, database=db, obsday_index=obsday_index

  ; close database connection
  if (~arg_present(db)) then obj_destroy, db
end


; main-level example program

@comp_config_common

date = '20130115'

comp_initialize, date
config_filename = filepath('comp.db.cfg', $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())
comp_configuration, config_filename=config_filename

obsday_index = mlso_obsday_insert(date, database_config_filename, database_config_section, $
                    database=db, status=status, log_name='comp')
comp_db_clearday, database=db, obsday_index=obsday_index

for w = 0L, n_elements(process_wavelengths) - 1L do begin
  comp_update_database, date, process_wavelengths[w], $
                        database=db, obsday_index=obsday_index
endfor

comp_cal_insert, date, database=db, obsday_index=obsday_index
obj_destroy, db

end
