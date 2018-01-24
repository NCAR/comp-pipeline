; docformat = 'rst'

;+
; Create CoMP database tables. Will destroy existing tables!
;
; :Keywords:
;   config_filename : in, required, type=string
;     filename of CoMP configuration file with "database" section with
;     "config_filename" and "config_section" options
;
; :Author:
;   MLSO Software Team
;-
pro comp_create_tables, config_filename=config_filename
  compile_opt strictarr

  log_name = 'comp'

  config = mg_read_config(config_filename)
  database_config_filename = config->get('config_filename', section='database')
  database_config_section = config->get('config_section', section='database')
  obj_destroy, config

  ; create MySQL database interface object
  db = mgdbmysql()
  db->connect, config_filename=database_config_filename, $
               config_section=database_config_section, $
               status=status, error_message=error_message
  if (status ne 0L) then begin
    mg_log, 'failed to connect to database', name=log_name, /error
    mg_log, '%s', error_message, name=log_name, /error
    return
  endif

  db->getProperty, host_name=host
  mg_log, 'connected to %s', host, name=log_name, /info


  tables = 'comp_' + ['file', 'sci', 'cal', 'eng', 'sw', 'mission', 'level']

  ; delete existing tables, if they exist
  for t = 0L, n_elements(tables) - 1L do begin
    mg_log, 'dropping %s...', tables[t], name=log_name, /info
    db->execute, 'drop table if exists %s', tables[t], $
                 status=status, error_message=error_message
    if (error ne 0L) then begin
      mg_log, 'problem dropping %s', tables[t], name=log_name, /error
      mg_log, '%s', error_message, name=log_name, /error
    endif
  endfor

  ; create tables
  for t = 0L, n_elements(tables) - 1L do begin
    mg_log, 'creating %s...', tables[t], name=log_name, /info

    definition_filename = filepath(string(tables[t], format='(%"%s.tbl")'), $
                                   root=mg_src_root())
    nlines = file_lines(definition_filename)
    sql_code = strarr(nlines)
    openr, lun, definition_filename, /get_lun
    readf, lun, sql_code
    free_lun, lun
    sql_code = strjoin(sql_code, mg_newline())

    db->execute, '%s', sql_code, $
                 status=status, error_message=error_message
    if (error ne 0L) then begin
      mg_log, 'problem creating %s', tables[t], name=log_name, /error
      mg_log, '%s', error_message, name=log_name, /error
    endif
  endfor

  ; disconnect from database
  mg_log, 'disconnecting from %s', host, name=log_name, /info
  obj_destroy, db
end


; main-level example program

comp_create_tables, config_filename=filepath('comp.mgalloy.mahi.latest.cfg', $
                                             subdir=['..', '..', 'config'], $
                                             root=mg_src_root())

end
