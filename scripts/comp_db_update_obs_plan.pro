; docformat = 'rst'

function comp_db_update_obs_plan_isint, value
  compile_opt strictarr

  on_ioerror, io_error

  !null = fix(value)
  return, 1B

  io_error:
  return, 0B
end


function comp_db_update_obs_plan_new_filename, basename, dateparts, producttype_id, rootdir
  compile_opt strictarr

  _dateparts = strjoin(dateparts, '/')

  file_parts = strsplit(d.file_name, '.', /extract)
  if (producttype_id eq 17 || producttype_id eq 18 || producttype_id eq 19) then begin
    new_basename = strjoin(file_parts[0:3], '.') + '.fts.gz'
    new_filename = filepath(new_basename, subdir=_dateparts, root=rootdir)
  endif else if (producttype_id eq 20 || producttype_id eq 21) then begin
    new_basename = strjoin(file_parts[0:4], '.') + '.?.fts.gz'
    new_filename = filepath(new_basename, subdir=_dateparts, root=rootdir)
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

  return, new_filename
end


pro comp_db_update_obs_plan_checkfile, filename, original_basename, $
                                       ntunes=ntunes, $
                                       obs_plan=obs_plan, $
                                       filesize=filesize
  compile_opt strictarr

  fits_open, filename, fcb
  fits_read, fcb, !null, primary_header, exten=0
  fits_close, fcb

  obs_plan = sxpar(primary_header, 'OBS_PLAN', count=n_obs_plan)
  if (n_obs_plan eq 0L) then begin
    if (n_elements(ntunes) eq 0L) then begin
      ntunes = sxpar(primary_header, 'NTUNES', count=n_ntunes)
      if (n_ntunes eq 0L) then begin
        filename_tokens = strsplit(original_basename, '.', /extract)
        ntunes = filename_tokens[-3]
        if (comp_db_update_obs_plan_isint(ntunes)) then begin
          ntunes = fix(ntunes)
        endif else begin
          basename = file_basename(filename)
          filename_tokens = strsplit(basename, '.', /extract)
          ntunes = filename_tokens[-3]
          if (comp_db_update_obs_plan_isint(ntunes)) then begin
            ntunes = fix(ntunes)
          endif else begin
            ; [TODO]: need to come up with an answer here
            ntunes = ?
          endelse
        endelse
      endif
    endif
    obs_plan = ntunes gt 3L ? 'synoptic' : 'waves'
  endif

  filesize = mg_filesize(filename)
end


pro comp_db_update_obs_plan
  compile_opt strictarr

  catch, error_no
  if (error_no ne 0L) then begin
    catch, /cancel
    if (n_elements(sql_cmd) ne 0L) then begin
      mg_log, sql_cmd, /critical
      help, obs_plan, d.file_name
    endif
    return
  endif
  
  database_config_filename = '~/.mysqldb'
  database_config_section = 'pipeline@databases'
  archive_rootdir = '/hao/acos'
  process_rootdir = '/hao/dawn/Data/CoMP/process'

  db = compdbmysql()
  db->connect, config_filename=database_config_filename, $
               config_section=database_config_section

  q = 'select file_id, file_name, producttype, ntunes, mlso_numfiles.obs_day from comp_file inner join mlso_numfiles on mlso_numfiles.day_id=comp_file.obs_day order by date_obs'
  result = db->query(q, count=n_rows)

  mg_log, 'found %d comp_file rows', n_rows, /info

  ; [TODO]: limiting for testing purposes
  n_rows = 10000L
  start_row = 0L

  n_updated = 0L
  for r = start_row, start_row + n_rows - 1L do begin
    mg_log, '%07d/%07d', r - start_row + 1, n_rows, /info

    published = 0L
    found = 0B
    d = result[r]

    date_parts = [strmid(d.obs_day, 0, 4), strmid(d.obs_day, 5, 2), strmid(d.obs_day, 8, 2)]
    filename = filepath(d.file_name, subdir=date_parts, root=archive_rootdir)
    if (file_test(filename, /regular)) then begin
      published = 1L
      found = 1B
      comp_db_update_obs_plan_checkfile, filename, $
                                         d.file_name, ntunes=d.ntunes, $
                                         obs_plan=obs_plan, filesize=filesize
      n_updated += 1L
    endif else begin
      new_filename = comp_db_update_obs_plan_new_filename(d.file_name, date_parts, d.producttype, archive_rootdir)
      if (file_test(new_filename, /regular)) then begin
        published = 1L
        found = 1B
        comp_db_update_obs_plan_checkfile, new_filename, $
                                           d.file_name, ntunes=d.ntunes, $
                                           obs_plan=obs_plan, filesize=filesize
        n_updated += 1L
      endif else begin
        filename = filepath(d.file_name, subdir=strjoin(date_parts, ''), root=process_rootdir)
        if (file_test(filename, /regular)) then begin
          published = 0L
          found = 1B
          comp_db_update_obs_plan_checkfile, filename, $
                                             d.file_name, ntunes=d.ntunes, $
                                             obs_plan=obs_plan, filesize=filesize
          n_updated += 1L
        endif else begin
          new_filename = comp_db_update_obs_plan_new_filename(d.file_name, strjoin(date_parts, ''), d.producttype, process_rootdir)
          if (file_test(new_filename, /regular)) then begin
            published = 0L
            found = 1B
            comp_db_update_obs_plan_checkfile, new_filename, $
                                               d.file_name, ntunes=d.ntunes, $
                                               obs_plan=obs_plan, filesize=filesize
            n_updated += 1L
          endif else begin
            mg_log, '%s not found', file_basename(new_filename), /error
          endelse
        endelse
      endelse
    endelse

    ; if (found) then begin
    ;   cmd = 'update comp_file set obs_plan=''%s'', published=%d where file_id=%d'
    ;   db->execute, cmd, obs_plan, published, d.file_id, $
    ;                sql_statement=sql_cmd
    ;   mg_log, 'set %s obs_plan to %s', d.file_name, obs_plan, /info
    ; endif
  endfor

  mg_log, '%d updated rows of %d', n_updated, n_rows, /info

  obj_destroy, db
end


; main-level example program

comp_db_update_obs_plan

end
