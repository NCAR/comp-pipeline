; docformat = 'rst'

pro comp_numfiles_update, date, database=db, obsday_index=obsday_index
  compile_opt strictarr
  @comp_config_common

  l1_dir = filepath('level1', subdir=date, root=process_basedir)
  l2_dir = filepath('level2', subdir=date, root=process_basedir)

  !null = file_search(filepath('*.*.comp.*.intensity.gif', $
                               root=l1_dir), $
                      count=n_intensity_fullresgif)

  !null = file_search(filepath('*.*.comp.*.polarization.fts.gz', $
                               root=l2_dir), $
                      count=n_polarization_fits)

  !null = file_search(filepath('*.*.comp.*.dynamics.fts.gz', $
                               root=l2_dir), $
                      count=n_dynamics_fits)

  !null = file_search(filepath('*.comp.????.{mean,median}.*.fts.gz', $
                               root=l2_dir), $
                      count=n_meanmedian_fits)

  !null = file_search(filepath('*.comp.????.quick_invert.*.fts.gz', $
                               root=l2_dir), $
                      count=n_quickinvert_fits)

  fields = [{name: 'num_comp_intensity_fullresgif', type: '%d'}, $
            {name: 'num_comp_polarization_fits', type: '%d'}, $
            {name: 'num_comp_dynamics_fits', type: '%d'}, $
            {name: 'num_comp_meanmedian_fits', type: '%d'}, $
            {name: 'num_comp_quickinvert_fits', type: '%d'}]
  sql_cmd = string(strjoin(fields.name + '=' + fields.type, ', '), $
                   obsday_index, $
                   format='(%"update mlso_numfiles set %s where day_id=%d")')
  db->execute, sql_cmd, $
               n_intensity_fullresgif, $
               n_polarization_fits, $
               n_dynamics_fits, $
               n_meanmedian_fits, $
               n_quickinvert_fits, $
               status=status, $
               error_message=error_message, $
               sql_statement=sql_cmd
  if (status ne 0L) then begin
    mg_log, 'error updating mlso_numfiles table', name='comp', /error
    mg_log, 'status: %d, error message: %s', status, error_message, $
            name='comp', /error
    mg_log, 'SQL command: %s', sql_cmd, name='comp', /error
  endif else begin
    mg_log, 'updated mlso_numfiles table', name='comp', /info
  endelse

  done:
  mg_log, 'done', name='comp', /info
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

comp_numfiles_update, date, database=db, obsday_index=obsday_index

obj_destroy, db

end
