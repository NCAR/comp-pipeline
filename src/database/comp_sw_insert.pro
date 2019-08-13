; docformat = 'rst'

;+
; Update comp_sw table.
;
; :Params:
;   date : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :Keywords:
;   database : in, required, type=MGdbMySQL object
;     database to insert rows into
;   obsday_index : in, required, type=long
;     observing day as index into mlso_numfiles table
;   sw_index : out, optional, type=long
;     set to a named variable to retrieve the comp_sw table index of the entry
;     just added (or already existing)
;
; :Author:
;   MLSO Software Team
;-
pro comp_sw_insert, date, wave_type, database=db, obsday_index=obsday_index, $
                    sw_index=sw_index
  compile_opt strictarr

  mg_log, 'starting...', name='comp', /info

  sw_index = 0L ; updated with correct index if all goes well
  sw_version = comp_find_code_version(revision=sw_revision)

  date_format = '(C(CYI, "-", CMOI2.2, "-", CDI2.2, "T", CHI2.2, ":", CMI2.2, ":", CSI2.2))'
  proc_date = string(julday(), format=date_format)
  
  ; check to see if passed observation day date is already in the kcor_sw table
  q = 'select count(sw_id) from comp_sw where sw_version=''%s'' and sw_revision=''%s'''
  sw_id_results = db->query(q, sw_version, sw_revision)
  sw_id_count = sw_id_results.count_sw_id_

  if (sw_id_count eq 0L) then begin
    mg_log, 'inserting a new comp_sw row...', name='comp', /info

    fields = [{name: 'obs_day', type: '''%s'''}, $
              {name: 'proc_date', type: '''%s'''}, $
              {name: 'sw_version', type: '''%s'''}, $
              {name: 'sw_revision', type: '''%s'''}]
    sql_cmd = string(strjoin(fields.name, ', '), $
                     strjoin(fields.type, ', '), $
                     format='(%"insert into comp_sw (%s) values (%s)")')
    db->execute, sql_cmd, $               
                 obsday_index, $
                 proc_date, $
                 sw_version, $
                 sw_revision, $
                 status=status, error_message=error_message, $
                 sql_statement=final_sql_cmd
    if (status ne 0L) then begin
      mg_log, '%d, error message: %s', status, error_message, $
              name='comp', /error
      mg_log, 'sql_cmd: %s', final_sql_cmd, name='comp', /error
      goto, done
    endif

    if (n_warnings gt 0L) then comp_db_log_warnings, database=db

    sw_index  = db->query('select last_insert_id()')
  endif else begin
    if (sw_id_count gt 1L) then begin
      mg_log, 'version %s, revision %s found %d times in comp_sw table', $
              sw_version, $
              sw_revision, $
              sw_id_count, $
              name='comp', /warn
    endif

    ; if it is in the database, get the corresponding sw_id
    q = 'select sw_id from comp_sw where sw_version=''%s'' and sw_revision=''%s'''
    sw_results = db->query(q, sw_version, sw_revision)
    sw_index = sw_results[0].sw_id
  endelse

  done:
  mg_log, 'done', name='comp', /info
end
