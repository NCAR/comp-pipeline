; docformat = 'rst'


;+
; Helper routine to clear a table for a given day.
;
; :Params:
;   table : in, required, type=string
;     table to clear, i.e., comp_file, comp_eng, etc.
;
; :Keywords:
;   obsday_index : in, required, type=integer
;     index into mlso_numfiles database table
;   database : in, optional, type=MGdbMySql object
;     database connection to use
;-
pro comp_db_clearday_cleartable, table, $
                                 obsday_index=obsday_index, $
                                 database=db
  compile_opt strictarr

  mg_log, 'clearing %s table', table, name='comp', /info
  db->execute, 'DELETE FROM %s WHERE obs_day=%d', $
               table, obsday_index, $
               status=status, error_message=error_message, sql_statement=sql_cmd, $
               n_affected_rows=n_affected_rows
  if (status ne 0L) then begin
    mg_log, 'error clearing %s table', table, name='comp', /error
    mg_log, 'status: %d, error message: %s', status, error_message, $
            name='comp', /error
    mg_log, 'SQL command: %s', sql_cmd, name='comp', /error
  endif else begin
    mg_log, '%d rows deleted', n_affected_rows, name='comp', /info
  endelse
end


;+
; Delete entries for a day, e.g., before reprocessing that day.
;
; :Keywords:
;   obsday_index : in, required, type=integer
;     index into mlso_numfiles database table
;   database : in, optional, type=MGdbMySql object
;     database connection to use
;   log_name : in, required, type=string
;     name of log to send log messages to
;   calibration : in, optional, type=boolean
;     set to just clear the calibration for a day
;-
pro comp_db_clearday, database=db, $
                      obsday_index=obsday_index, $
                      calibration=calibration
  compile_opt strictarr

  db->getProperty, host_name=host
  mg_log, 'using connection to %s', host, name='comp', /debug

  day = db->query('select * from mlso_numfiles where day_id=%d', obsday_index, $
                  status=status, error_message=error_message, sql_statement=sql_cmd)
  if (status ne 0L) then begin
    mg_log, 'error querying mlso_numfiles table', name='comp', /error
    mg_log, 'status: %d, error message: %s', status, error_message, $
            name='comp', /error
    mg_log, 'SQL command: %s', sql_cmd, name='comp', /error
  endif

  mg_log, 'clearing entries for obsday index %d (%s)', $
          obsday_index, day[0].obs_day, $
          name='comp', /info

  ; zero CoMP fields in mlso_numfiles
  if (not keyword_set(calibration)) then begin
    mg_log, 'zeroing CoMP values for mlso_numfiles table', name='comp', /info

    fields = 'num_comp_' + ['intensity_fullresgif', $
                            'polarization_fits', $
                            'dynamics_fits', $
                            'meanmedian_fits', $
                            'quickinvert_fits']
    fields_expression = strjoin(fields + '=0', ', ')
    db->execute, 'update mlso_numfiles set %s where day_id=''%d''', $
                 fields_expression, $
                 obsday_index, $
                 status=status, error_message=error_message, sql_statement=sql_cmd
    if (status ne 0L) then begin
      mg_log, 'error zeroing values in mlso_numfiles table', name='comp', /error
      mg_log, 'status: %d, error message: %s', status, error_message, $
              name='comp', /error
      mg_log, 'SQL command: %s', sql_cmd, name='comp', /error
    endif

    comp_db_clearday_cleartable, 'comp_file', $
                                 obsday_index=obsday_index, $
                                 database=db
    comp_db_clearday_cleartable, 'comp_sci', $
                                 obsday_index=obsday_index, $
                                 database=db
    comp_db_clearday_cleartable, 'comp_dynamics', $
                                 obsday_index=obsday_index, $
                                 database=db
    comp_db_clearday_cleartable, 'comp_eng', $
                                 obsday_index=obsday_index, $
                                 database=db

  endif

  comp_db_clearday_cleartable, 'comp_cal', $
                               obsday_index=obsday_index, $
                               database=db

  done:
  if (~arg_present(db)) then obj_destroy, db
  mg_log, 'done', name='comp', /info
end
