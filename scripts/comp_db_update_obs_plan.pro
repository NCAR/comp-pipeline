; docformat = 'rst'

pro comp_db_update_obs_plan
  compile_opt strictarr

  database_config_filename = '~/.mysqldb'
  database_config_section = 'pipeline@databases'
  archive_rootdir = '/hao/acos'

  db = compdbmysql()
  db->connect, config_filename=database_config_filename, $
               config_section=database_config_section

  q = 'select file_id, file_name, producttype, mlso_numfiles.obs_day from comp_file inner join mlso_numfiles on mlso_numfiles.day_id=comp_file.obs_day where obs_plan is NULL order by date_obs'
  
  result = db->query(q, count=n_rows)

  mg_log, 'found %d NULL obs_plan rows', n_rows, /info

  updated = 0L
  for r = 0L, n_rows - 1L do begin
    d = result[r]
    date_parts = [strmid(d.obs_day, 0, 4), strmid(d.obs_day, 5, 2), strmid(d.obs_day, 8, 2)]
    filename = filepath(d.file_name, subdir=date_parts, root=archive_rootdir)
    if (file_test(filename, /regular)) then begin
      fits_open, filename, fcb
      fits_read, fcb, !null, primary_header, exten=0
      fits_close, fcb

      obs_plan = sxpar(primary_header, 'OBS_PLAN')
      
      cmd = 'update comp_file set obs_plan=''%s'' where file_id=%d'
      db->execute, cmd, obs_plan, d.file_id
      mg_log, 'set %s obs_plan to %d', d.file_name, obs_plan, /info
      updated += 1L
    endif else begin
      file_parts = strsplit(d.file_name, '.', /extract)
      if (d.producttype eq 17 || d.producttype eq 18 || d.producttype eq 19) then begin
        new_basename = strjoin(file_parts[0:3], '.') + '.fts.gz'
        new_filename = filepath(new_basename, subdir=date_parts, root=archive_rootdir)
      endif else if (d.producttype eq 20 || d.producttype eq 21) then begin
        new_basename = strjoin(file_parts[0:4], '.') + '.?.fts.gz'
        new_filename = filepath(new_basename, subdir=date_parts, root=archive_rootdir)
        candidates = file_search(new_filename, count=n_candidates)
        if (n_candidates eq 0L) then begin
          mg_log, 'cannot find dynamics/polarization file', /error
          continue
        endif
        new_filename = candidates[0]
      endif else begin
        mg_log, 'cannot fix quick invert or mean/median', /error
        continue
      endelse
      
      if (file_test(new_filename, /regular)) then begin
        fits_open, new_filename, fcb
        fits_read, fcb, !null, primary_header, exten=0
        fits_close, fcb

        obs_plan = sxpar(primary_header, 'OBS_PLAN')
      
        cmd = 'update comp_file set obs_plan=''%s'' where file_id=%d'
        db->execute, cmd, obs_plan, d.file_id
        mg_log, 'set %s obs_plan to %d', d.file_name, obs_plan, /warn
      endif else begin
        mg_log, '%s not found', file_basename(new_filename), /error
      endelse
    endelse
  endfor

  mg_log, '%d updated rows of %d', updated, n_rows, /info

  obj_destroy, db
end


; main-level example program

comp_db_update_obs_plan

end
