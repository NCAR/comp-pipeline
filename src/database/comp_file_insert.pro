; docformat = 'rst'

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

  l1_files = comp_find_l1_files(date_dir, wave_type, /all, count=n_l1_files)

  mg_log, 'inserting %d L1 files into comp_file table...', n_l1_files, $
          name='comp', /info

  level_id = comp_get_level_id('l1', database=db)
  filetype_id = comp_get_filetype_id('fits', database=db)

  ; TODO: is it intensity, or spectropolarimetry, or do we need to add something
  ; to the product types?
  producttype_id = comp_get_producttype_id('intentsity', database=db)

  for f = 0L, n_l1_files - 1L do begin
    fits_open, l1_files[f], fcb
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
                 file_basename(l1_files[f]), $
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
  endfor

  ; TODO: handle L2 files

  done:
  mg_log, 'done', name='comp', /info
end
