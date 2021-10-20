; docformat = 'rst'

pro comp_db_update_gbu, start_date, end_date, config_filename
  compile_opt strictarr
  @comp_config_common

  comp_configuration, config_filename=config_filename

  wave_types = ['1074', '1079', '1083']

  db = compdbmysql()
  db->connect, config_filename=database_config_filename, $
               config_section=database_config_section

  date = start_date
  while (date ne end_date) do begin
    for w = 0L, n_elements(wave_types) - 1L do begin
      gbu_basename = string(date, wave_types[w], format='(%"%s.comp.%s.gbu.log")')
      gbu_filename = filepath(gbu_basename, $
                              subdir=[date, 'level1'], $
                              root=process_basedir)
      if (~file_test(gbu_filename, /regular)) then continue

      print, gbu_basename, format='(%"updating from %s...")'
      gbu = comp_read_gbu(gbu_filename, count=n_files)
      l1_files = gbu.l1file
      reasons = gbu.reason

      for f = 0L, n_files - 1L do begin
        db->execute, 'update comp_file set gbu=%d where file_name = ''%s''', $
                     reasons[f], $
                     l1_files[f], $
                     status=status, $
                     error_message=error_message
        if (status ne 0L) then print, error_message
      endfor
    endfor

    date = comp_increment_date(date)
  endwhile

  obj_destroy, db
end


; main-level example program

config_basename = 'comp.reprocess.cfg'
config_filename = filepath(config_basename, $
                           subdir=['..', 'config'], $
                           root=mg_src_root())
;comp_db_update_gbu, '20121201', '20180404', config_filename
comp_db_update_gbu, '20121201', '20121202', config_filename

end
