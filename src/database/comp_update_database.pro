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
    db = compdbmysql()
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

  comp_file_insert, date, wave_type, $
                    database=db, obsday_index=obsday_index
  comp_eng_insert, date, wave_type, $
                   database=db, obsday_index=obsday_index
  comp_sci_insert, date, wave_type, $
                   database=db, obsday_index=obsday_index
  comp_dynamics_insert, date, wave_type, $
                        database=db, obsday_index=obsday_index

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
