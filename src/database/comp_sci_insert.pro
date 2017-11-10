; docformat = 'rst'

;+
; Update comp_sci table with results of pipeline run for the given day.
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
pro comp_sci_insert, date, wave_type, database=db, obsday_index=obsday_index
  compile_opt strictarr

  mg_log, 'inserting rows into comp_sci table...', name='comp', /info

  ; find L1 files
  l1_files = comp_find_l1_files(date_dir, wave_type, /all, count=n_l1_files)
  
  ; angles for full circle in radians
  theta = findgen(360) * !dtor

  ; loop through L1 files
  for f = 0L, n_l1_files - 1L do begin
    fits_open, l1_files[f], fcb
    fits_read, fcb, data, primary_header, exten_no=0
    date_obs = strin(sxpar(primary_header, 'DATE-OBS'), $
                     sxpar(primary_header, 'TIME-OBS'), $
                     format='(%"%sT%s")')

    ; insert into comp_sci table
    db->execute, 'INSERT INTO comp_sci (file_name, date_obs, obs_day, totali, totalq, totalu, intensity, intensity_stddev, q, q_stddev, u, u_stddev, r108i, r13i, r108l, r13l, r108radazi, r13radazi) VALUES (''%s'', ''%s'', %d, %f, %f, %f, ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'')', $
                 file_basename(l1_files[f], '.gz'), $
                 date_obs, $
                 obsday_index, $
                 total_i, total_q, total_u, $
                 db->escape_string(intensity), $
                 db->escape_string(intensity_stdev), $
                 db->escape_string(q), $
                 db->escape_string(q_stddev), $
                 db->escape_string(u), $
                 db->escape_string(u_stddev), $
                 db->escape_string(r108i), $
                 db->escape_string(r13i), $
                 db->escape_string(r108l), $
                 db->escape_stringr(r13l), $
                 db->escape_string(r108radazi), $
                 db->escape_string(r13radazi), $
                 status=status, $
                 error_message=error_message, $
                 sql_statement=sql_cmd, $
                 n_warnings=n_warnings
    if (status ne 0L) then begin
      mg_log, 'error inserting into comp_sci table', name='comp', /error
      mg_log, 'status: %d, error message: %s', status, error_message, $
              name='comp', /error
      mg_log, 'SQL command: %s', sql_cmd, name='comp', /error
    endif

    if (n_warnings gt 0L) then comp_db_log_warnings, database=db

    fits_close, fcb
  endfor

  mg_log, 'done', name='comp', /info
end
