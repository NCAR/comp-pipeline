; docformat = 'rst'

pro comp_file_insert_l2type, date, wave_type, fname_type, $
                             product_type=product_type, $
                             database=db, $
                             obsday_index=obsday_index
  compile_opt strictarr

  basename = string(date, wave_type, fname_type, format='(%"%s.comp.%s.%s.*fts*")')
  files = file_search(filepath(basename, $
                               subdir=[date, 'level2'], $
                               root=process_basedir), $
                      count=n_files)
  for f = 0L, n_files - 1L do begin
    comp_file_insert_file, files[f], wave_type, $
                           product_type=product_type, $
                           database=db, $
                           obsday_index=obsday_index
  endfor
end


pro comp_file_insert_file, file, wave_type, level_id, filetype_id, $
                           product_type=product_type, $
                           database=db, $
                           obsday_index=obsday_index
  compile_opt strictarr

  fits_open, file, fcb
  fits_read, fcb, data, primary_header, exten_no=0, /no_abort, message=msg, $
             /header_only
  fits_close, fcb
  if (msg ne '') then message, msg

  date_obs = string(sxpar(primary_header, 'DATE-OBS'), $
                    sxpar(primary_header, 'TIME-OBS'), $
                    format='(%"%sT%s")')

  carrington_rotation = sxpar(primary_header, 'CARR_ROT')

  pol_list = strtrim(sxpar(primary_header, 'POL_LIST'), 2)
  ntunes = sxpar(primary_header, 'NTUNES')

  if (n_elements(product_type) eq 0L) then begin
    producttype_id = comp_get_producttype_id(strtrim(pol_list, 2), database=db)
  endif else begin
    producttype_id = comp_get_producttype_id(product_type, database=db)
  endelse

  ; TODO: what is quality?
  quality = 75

  fields = [{name: 'file_name', type: '''%s'''}, $
            {name: 'date_obs', type: '''%s'''}, $
            {name: 'obs_day', type: '%d'}, $
            {name: 'carrington_rotation', type: '%d'}, $
            {name: 'level', type: '%d'}, $
            {name: 'producttype', type: '%d'}, $
            {name: 'filetype', type: '%d'}, $
            {name: 'quality', type: '%d'}, $
            {name: 'pol_list', type: '''%s'''}, $
            {name: 'wavetype', type: '%d'}, $
            {name: 'ntunes', type: '%d'}]
  sql_cmd = string(strjoin(fields.name, ', '), $
                   strjoin(fields.type, ', '), $
                   format='(%"insert into comp_file (%s) values (%s)")')
  db->execute, sql_cmd, $
               file_basename(file), $
               date_obs, $
               obsday_index, $
               carrington_rotation, $
               level_id, $
               producttype_id, $
               filetype_id, $
               quality, $
               pol_list, $
               long(wave_type), $
               ntunes, $
               status=status, $
               error_message=error_message, $
               sql_statement=final_sql_cmd, $
               n_warnings=n_warnings
  if (status ne 0L) then begin
    mg_log, 'error inserting into comp_file table', name='comp', /error
    mg_log, 'status: %d, error message: %s', status, error_message, $
            name='comp', /error
    mg_log, 'SQL command: %s', final_sql_cmd, name='comp', /error
  endif

  if (n_warnings gt 0L) then comp_db_log_warnings, database=db
end


;+
; Update comp_file table.
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
;
; :Author:
;   MLSO Software Team
;-
pro comp_file_insert, date, wave_type, database=db, obsday_index=obsday_index
  compile_opt strictarr

  l1_files = comp_find_l1_file(date, wave_type, /all, count=n_l1_files)

  if (n_l1_files gt 0L) then begin
    mg_log, 'inserting %d L1 files into %s nm comp_file table...', $
            n_l1_files, wave_type, $
            name='comp', /info
  endif else begin
    mg_log, 'no L1 files to insert into %s nm comp_file table...', $
            wave_type, $
            name='comp', /info
  endelse

  level_id = comp_get_level_id('l1', database=db)
  filetype_id = comp_get_filetype_id('fits', database=db)

  for f = 0L, n_l1_files - 1L do begin
    comp_file_insert_file, l1_files[f], wave_type, level_id, filetype_id, $
                           database=db, obsday_index=obsday_index
  endfor

  ; handle L2 files
  comp_file_insert_l2type, date, wave_type, 'dynamics', $
                           product_type='dynamics', $
                           database=db, $
                           obsday_index=obsday_index
  comp_file_insert_l2type, date, wave_type, 'polarization', $
                           product_type='polarization', $
                           database=db, $
                           obsday_index=obsday_index
  comp_file_insert_l2type, date, wave_type, 'quick_invert', $
                           product_type='quick-invert', $
                           database=db, $
                           obsday_index=obsday_index
  comp_file_insert_l2type, date, wave_type, 'mean', $
                           product_type='mean', $
                           database=db, $
                           obsday_index=obsday_index
  comp_file_insert_l2type, date, wave_type, 'median', $
                           product_type='median', $
                           database=db, $
                           obsday_index=obsday_index
  comp_file_insert_l2type, date, wave_type, 'sigma', $
                           product_type='sigma', $
                           database=db, $
                           obsday_index=obsday_index

  done:
  mg_log, 'done', name='comp', /info
end
