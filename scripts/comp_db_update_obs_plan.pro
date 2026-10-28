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

  file_parts = strsplit(basename, '.', /extract)
  if (producttype_id eq 17 || producttype_id eq 18 || producttype_id eq 19) then begin
    new_basename = strjoin(file_parts[0:3], '.') + '.fts.gz'
    new_filename = filepath(new_basename, subdir=_dateparts, root=rootdir)
  endif else if (producttype_id eq 20 || producttype_id eq 21) then begin
    new_basename = strjoin(file_parts[0:4], '.') + '.?.fts.gz'
    new_filename = filepath(new_basename, subdir=_dateparts, root=rootdir)
    candidates = file_search(new_filename, count=n_candidates)
    if (n_candidates eq 0L) then begin
      new_filename = ''
    endif
    new_filename = candidates[0]
  endif else begin
    new_filename = ''
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
            ntunes = ''
          endelse
        endelse
      endif
    endif
    obs_plan = ntunes gt 3L ? 'synoptic' : 'waves'
  endif

  filesize = mg_filesize(filename)
end


pro comp_db_update_obs_plan, start_row=start_row, $
                             n_rows_to_process=n_rows_to_process
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

  update = 1B

  database_config_filename = '~/.mysqldb'
  database_config_section = 'pipeline@databases'
  archive_rootdir = '/hao/acos'
  process_rootdir = '/hao/dawn/Data/CoMP/process'

  db = compdbmysql()
  db->connect, config_filename=database_config_filename, $
               config_section=database_config_section

  q = 'select file_id, file_name, level, producttype, ntunes, mlso_numfiles.obs_day from comp_file inner join mlso_numfiles on mlso_numfiles.day_id=comp_file.obs_day order by date_obs'
  result = db->query(q, count=n_rows)

  mg_log, 'found %d comp_file rows', n_rows, /info

  ; [TODO]: limiting for testing purposes
  start_row = mg_default(start_row, 0L)
  n_rows_to_process = mg_default(n_rows_to_process, n_rows - start_row)

  n_updated = 0L
  n_published = 0L
  for r = start_row, start_row + n_rows_to_process - 1L do begin
    mg_log, '%07d/%07d', r + 1, start_row + n_rows_to_process, /info

    published = 0L
    found = 0B
    file_name = ''
    d = result[r]

    d_file_name = d.file_name + (strmid(d.file_name, 2, /reverse_offset) eq '.gz' ? '' : '.gz')
    date_parts = [strmid(d.obs_day, 0, 4), strmid(d.obs_day, 5, 2), strmid(d.obs_day, 8, 2)]
    filename = filepath(d_file_name, subdir=date_parts, root=archive_rootdir)
    obs_plan = ''
    if (file_test(filename, /regular)) then begin
      published = 1L
      found = 1B
      file_name = d_file_name
      comp_db_update_obs_plan_checkfile, filename, $
                                         d_file_name, ntunes=d.ntunes, $
                                         obs_plan=obs_plan, filesize=filesize
    endif else begin
      new_filename = comp_db_update_obs_plan_new_filename(d_file_name, date_parts, d.producttype, archive_rootdir)
      if (file_test(new_filename, /regular)) then begin
        published = 1L
        found = 1B
        file_name = file_basename(new_filename)
        comp_db_update_obs_plan_checkfile, new_filename, $
                                           d_file_name, ntunes=d.ntunes, $
                                           obs_plan=obs_plan, filesize=filesize
      endif else begin
        level_name = d.level eq 2 ? 'level1' : 'level2'
        filename = filepath(d_file_name, subdir=[strjoin(date_parts, ''), level_name], root=process_rootdir)
        file_name = d_file_name
        if (file_test(filename, /regular)) then begin
          published = 0L
          found = 1B
          comp_db_update_obs_plan_checkfile, filename, $
                                             d_file_name, ntunes=d.ntunes, $
                                             obs_plan=obs_plan, filesize=filesize
        endif else begin
          new_filename = comp_db_update_obs_plan_new_filename( $
            d_file_name, $
            strjoin(date_parts, '') + '/' + level_name, $
            d.producttype, $
            process_rootdir $
          )
          file_name = file_basename(new_filename)
          if (file_test(new_filename, /regular)) then begin
            published = 0L
            found = 1B
            comp_db_update_obs_plan_checkfile, new_filename, $
                                               d_file_name, ntunes=d.ntunes, $
                                               obs_plan=obs_plan, filesize=filesize
          endif else begin
            mg_log, '%s not found', file_basename(d_file_name), /error
          endelse
        endelse
      endelse
    endelse

    obs_plan = n_elements(obs_plan) eq 0L ? '' : strtrim(obs_plan, 2)

    if (found) then begin
      if (update) then begin
        cmd = 'update comp_file set file_name=''%s'', filesize=%d, obs_plan=''%s'', published=%d where file_id=%d'
        db->execute, cmd, file_name, filesize, obs_plan, published, d.file_id, $
                    sql_statement=sql_cmd, status=status
        if (status eq 0L) then begin
          mg_log, sql_cmd, /debug
        endif else begin
          mg_log, 'problem with SQL command (status: %d)', status, /error
          mg_log, sql_cmd, /error
        endelse
      endif
      n_updated += 1L
    endif else begin
      mg_log, 'cannot find %s', d.file_name, /error
    endelse
    if (published) then n_published += 1
  endfor

  mg_log, '%d updated rows of %d', n_updated, n_rows_to_process, /info
  mg_log, '%d published rows of %d', n_published, n_rows_to_process, /info

  obj_destroy, db
end


; main-level example program

comp_db_update_obs_plan

end
