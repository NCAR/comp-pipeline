; docformat = 'rst'

pro comp_db_update_filesize
  compile_opt strictarr

  database_config_filename = '~/.mysqldb'
  database_config_section = 'pipeline@databases'
  archive_rootdir = '/hao/acos'

  db = compdbmysql()
  db->connect, config_filename=database_config_filename, $
               config_section=database_config_section

  q = 'select file_id, file_name, mlso_numfiles.obs_day from comp_file inner join mlso_numfiles on mlso_numfiles.day_id=comp_file.obs_day where filesize is NULL'
  
  result = db->query(q, count=n_rows)

  mg_log, 'found %d NULL filesizes', n_rows, /info

  for r = 0L, n_rows - 1L do begin
    d = result[r]
    date_parts = [strmid(d.obs_day, 0, 4), strmid(d.obs_day, 5, 2), strmid(d.obs_day, 8, 2)]
    filename = filepath(d.file_name, subdir=date_parts, root=archive_rootdir)
    if (file_test(filename, /regular)) then begin
      filesize = mg_filesize(filename)

      cmd = 'update comp_file set filesize=%d where file_id=%d'
      db->execute, cmd, filesize, d.file_id
      mg_log, 'set %s filesize to %d', d.file_name, filesize, /info
    endif else begin
      mg_log, '%s not found', d.file_name, /warn
    endelse
  endfor

  obj_destroy, db
end


; main-level example program

comp_db_update_filesize

end
