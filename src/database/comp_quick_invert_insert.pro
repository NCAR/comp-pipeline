; docformat = 'rst'

;+
; Update comp_file table with results of pipeline run for the given day.
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
pro comp_quick_invert_insert, date, wave_type, $
                              database=db, obsday_index=obsday_index
  compile_opt strictarr
  @comp_config_common

  basename = string(date, wave_type, format='(%"%s.comp.%s.quick_invert.*.fts*")')
  quick_invert_files = file_search(filepath(basename, $
                                            subdir=[date, 'level2'], $
                                            root=process_basedir), $
                                   count=n_quick_invert_files)

  if (n_quick_invert_files gt 0L) then begin
    mg_log, 'inserting %d rows into %s nm comp_file table...', $
            n_quick_invert_files, wave_type, $
            name='comp', /info
  endif else begin
    mg_log, 'no quick_invert files to insert into %s nm comp_file table', $
            wave_type, $
            name='comp', /info
    goto, done
  endelse

  ; get quick_invert product type from mlso_producttype
  producttype = comp_get_producttype_id('quick-invert', database=db)

  ; get file type from mlso_filetype
  filetype = comp_get_filetype_id('fits', database=db)

  ; get level from comp_level
  level = comp_get_level_id('L2', database=db)

  quality = 100

  ; loop through quick_invert files to insert into comp_img
  for f = 0L, n_quick_invert_files - 1L do begin
    mg_log, '%d/%d: inserting %s', $
            f + 1, n_quick_invert_files, file_basename(quick_invert_files[f]), $
            name='comp', /debug
    fits_open, quick_invert_files[f], fcb
    fits_read, fcb, data, primary_header, exten_no=0, /no_abort, message=msg
    fits_read, fcb, intensity, intensity_header, exten_no=1, /no_abort, message=msg
    fits_read, fcb, velocity, velocity_header, exten_no=3, /no_abort, message=msg
    fits_close, fcb
    if (msg ne '') then message, msg

    date_obs = string(sxpar(primary_header, 'DATE-OBS'), $
                      sxpar(primary_header, 'TIME-OBS'), $
                      format='(%"%sT%s")')
    date_obs = comp_normalize_datetime(date_obs)

    carrington_rotation = sxpar(primary_header, 'CARR_ROT')
    pol_list = strtrim(sxpar(primary_header, 'POL_LIST'), 2)
    ntunes = sxpar(primary_header, 'NTUNES')

    ; insert into comp_file table
    fields = [{name: 'file_name', type: '''%s'''}, $
              {name: 'date_obs', type: '''%s'''}, $
              {name: 'obs_day', type: '%d'}, $

              {name: 'carrington_rotation', type: '%d'}, $
              {name: 'level', type: '%d'}, $
              {name: 'producttype', type: '%d'}, $
              {name: 'filetype', type: '%d'}, $
              {name: 'quality', type: '%d'}, $
              {name: 'pol_list', type: '''%s'''}, $
              {name: 'wavetype', type: '''%s'''}, $
              {name: 'ntunes', type: '%d'}]
    sql_cmd = string(strjoin(fields.name, ', '), $
                     strjoin(fields.type, ', '), $
                     format='(%"insert into comp_file (%s) values (%s)")')

    db->execute, sql_cmd, $
                 file_basename(quick_invert_files[f], '.gz'), $
                 date_obs, $
                 obsday_index, $

                 carrington_rotation, $
                 level, $
                 producttype, $
                 filetype, $
                 quality, $
                 pol_list, $
                 wave_type, $
                 ntunes, $

                 status=status, $
                 error_message=error_message, $
                 sql_statement=sql_query
    if (status ne 0L) then begin
      mg_log, 'comp_file status: %d', status, name='comp', /warn
      mg_log, 'error: %s', error_message, name='comp', /warn
      mg_log, 'sql_query: %s', sql_query, name='comp', /warn
    endif
  endfor

  done:
  mg_log, 'done', name='comp', /info
end


; main-level example program
@comp_config_common

date = '20180101'

comp_initialize, date
config_filename = filepath('comp.db.cfg', $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())
comp_configuration, config_filename=config_filename

; get database connection
db = compdbmysql()
db->connect, config_filename=database_config_filename, $
             config_section=database_config_section, $
             status=status, error_message=error_message

; get obs day index
obsday_index = mlso_obsday_insert(date, database_config_filename, database_config_section, $
                                  database=db, status=status)

print, date, obsday_index, format='Obsday index for %s is %d'

comp_quick_invert_insert, date, '1074', $
                          database=db, obsday_index=obsday_index
comp_quick_invert_insert, date, '1079', $
                          database=db, obsday_index=obsday_index

obj_destroy, db

end
