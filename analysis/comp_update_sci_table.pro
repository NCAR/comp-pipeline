; docformat = 'rst'

pro comp_update_sci_table, start_date, end_date
  compile_opt strictarr
  @comp_config_common
  
  wave_type = '1074'

  date = start_date
  wave_types = ['1074', '1079', '1083']

  while (date ne end_date) do begin
    mg_log, 'updating %s', date, name='comp', /info
    comp_initialize, date
    comp_update_configuration, date

    obsday_index = mlso_obsday_insert(date, $
                                      database_config_filename, $
                                      database_config_section, $
                                      database=db, $
                                      status=status, $
                                      log_name='comp')

    db->execute, 'delete from comp_sci where obs_day=%d', $
                 obsday_index, $
                 status=status, $
                 error_message=error_message, $
                 sql_statement=sql_cmd, $
                 n_affected_rows=n_affected_rows
    if (status ne 0L) then begin
      mg_log, 'error clearing %s table', table, name='comp', /error
      mg_log, 'status: %d, error message: %s', status, error_message, $
              name='comp', /error
      mg_log, 'SQL command: %s', sql_cmd, name='comp', /error
    endif else begin
      mg_log, '%d rows deleted', n_affected_rows, name='comp', /info
    endelse

    for w = 0L, n_elements(wave_types) - 1L do begin
      comp_sci_insert, date, wave_types[w], database=db, obsday_index=obsday_index
    endfor

    date = comp_increment_date(date)
    obj_destroy, db
  endwhile
end


; main-level example program

@comp_config_common


config_filename = filepath('comp.reprocess.cfg', $
                           subdir=['..', 'config'], $
                           root=mg_src_root())

; start_date = '20121201'
; end_date = '20180407'
; comp_update_sci_table, start_date, end_date

comp_configuration, config_filename=config_filename

db = compdbmysql()
db->connect, config_filename=database_config_filename, $
             config_section=database_config_section, $
             status=status, error_message=error_message

wave_types = ['1074', '1079', '1083']
for w = 0L, n_elements(wave_types) - 1L do begin
  q = 'select * from comp_sci where wavetype=\"%s\"'
  rows = db->query(q, wave_types[w], status=status, error_message=error_message, count=n_rows)
  if (n_rows eq 0L) then continue

  jds = comp_dateobs2jd(rows.date_obs)

  !null = label_date(date_format='%Y-%N-%D')

  window, xsize=1000, ysize=300, /free, $
          title=string(wave_types[w], format='(%"%s nm Intensity")')
  plot, jds, rows.r110_intensity_mean, psym=4, symsize=0.5, $
        title='Intensity in annulus 1.08-1.13 Rsun', $
        xstyle=1, xticks=6, xtickformat='label_date', $
        ytitle='1e-6 B/Bsun'

  window, xsize=1000, ysize=300, /free, $
          title=string(wave_types[w], format='(%"%s nm Background")')
  plot, jds, rows.r110_background_mean, psym=4, symsize=0.5, $
        title='Background in annulus 1.08-1.13 Rsun', $
        xstyle=1, xticks=6, xtickformat='label_date', $
        ytitle='1e-6 B/Bsun'
endfor

obj_destroy, db

end
