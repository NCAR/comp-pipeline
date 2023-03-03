; docformat = 'rst'

pro comp_db_update_l2, start_date, end_date, config_filename
  compile_opt strictarr
  @comp_config_common

  comp_configuration, config_filename=config_filename

  wave_types = ['1074', '1079']

  db = compdbmysql()
  db->connect, config_filename=database_config_filename, $
               config_section=database_config_section

  date = start_date
  while (date ne end_date) do begin
    obsday_index = mlso_obsday_insert(date, $
                                      database_config_filename, $
                                      database_config_section, $
                                      database=db, status=status)
    for w = 0L, n_elements(wave_types) - 1L do begin
      comp_dynamics_insert, date, wave_types[w], $
                            database=db, obsday_index=obsday_index
      comp_polarization_insert, date, wave_types[w], $
                                database=db, obsday_index=obsday_index
      comp_quick_invert_insert, date, wave_types[w], $
                                database=db, obsday_index=obsday_index
      comp_average_insert, date, wave_types[w], 'mean', $
                           database=db, obsday_index=obsday_index
      comp_average_insert, date, wave_types[w], 'median', $
                           database=db, obsday_index=obsday_index
    endfor

    date = comp_increment_date(date)
  endwhile

  obj_destroy, db
end


; main-level example program

config_basename = 'comp.db.cfg'
config_filename = filepath(config_basename, $
                           subdir=['..', 'config'], $
                           root=mg_src_root())

comp_db_update_l2, '20120101', '20180101', config_filename
comp_db_update_l2, '20180102', '20180406', config_filename

end
