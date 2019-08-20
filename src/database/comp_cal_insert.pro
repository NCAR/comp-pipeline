; docformat = 'rst'

;+
; Update comp_cal table.
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
pro comp_cal_insert, date, database=db, obsday_index=obsday_index
  compile_opt strictarr
  @comp_config_common

  level_id = comp_get_level_id('L1', database=db)

  ; find cal, dark, and flat files from listings:
  ;   - YYYYMMDD.comp.cal.files.txt
  ;   - YYYYMMDD.comp.dark.files.txt
  ;   - YYYYMMDD.comp.opal.files.txt
  types = ['cal', 'dark', 'opal']
  fmt = string(date, format='(%"%s.comp.%%s.files.txt")')
  catalog_basenames = string(types, format='(%"' + fmt + '")')
  catalog_filenames = filepath(catalog_basenames, $
                               subdir=[date, 'level1'], $
                               root=process_basedir)
  for c = 0L, n_elements(types) - 1L do begin
    n_files = file_lines(catalog_filenames[c])

    if (n_files eq 0L) then begin
      mg_log, 'no %s files to insert into comp_cal table', $
              types[c], $
              name='comp', /info
      continue
    endif else begin
      mg_log, 'inserting %d %s files into comp_cal table', $
              n_files, types[c], $
              name='comp', /info
    endelse

    files = strarr(n_files)
    openr, lun, catalog_filenames[c], /get_lun
    readf, lun, files
    free_lun, lun

    cal_basenames = strmid(files, 0, 19)
    for f = 0L, n_files - 1L do begin
      filename = filepath(cal_basenames[f], subdir=date, root=raw_basedir)

      fits_open, filename, fcb
      fits_read, fcb, data, primary_header, exten_no=0, /no_abort, message=msg, $
                 /header_only
      fits_read, fcb, data, header, exten_no=1, /no_abort, message=msg
      fits_close, fcb
      if (msg ne '') then message, msg

      date_obs = string(sxpar(primary_header, 'DATE-OBS'), $
                        sxpar(primary_header, 'TIME-OBS'), $
                        format='(%"%sT%s")')

      ntunes = sxpar(primary_header, 'NTUNES')
      cover = sxpar(primary_header, 'COVER')
      opal = sxpar(primary_header, 'OPAL')
      polangle = sxpar(primary_header, 'POLANGLE')
      polarizer = sxpar(primary_header, 'POLARIZR')
      retarder = sxpar(primary_header, 'RETARDER')

      occulter_id = sxpar(primary_header, 'OCC-ID')

      wavelength = comp_find_wave_type(sxpar(header, 'WAVELENG'))
      exposure = sxpar(header, 'EXPOSURE')
      ndfilter = sxpar(header, 'NDFILTER')

      ; TODO: compute from dark corrected annulus (read from YYYYMMDD.flat.fts?)
      median_int_continuum = 0.0
      median_int_linecenter = 0.0

      fields = [{name: 'file_name', type: '''%s'''}, $
                {name: 'date_obs', type: '''%s'''}, $
                {name: 'obs_day', type: '%d'}, $

                {name: 'wavelength', type: '%f'}, $
                {name: 'n_points', type: '%d'}, $
                {name: 'level', type: '%s'}, $
                {name: 'exposure', type: '%f'}, $
                {name: 'nd', type: '%d'}, $

                {name: 'cover', type: '%d'}, $
                {name: 'opal', type: '%d'}, $
                {name: 'polangle', type: '%f'}, $
                {name: 'polarizer', type: '%d'}, $
                {name: 'retarder', type: '%d'}, $

                {name: 'median_int_continuum', type: '%f'}, $
                {name: 'median_int_linecenter', type: '%f'}, $

                {name: 'occulter_id', type: '''%s'''}]
      sql_cmd = string(strjoin(fields.name, ', '), $
                       strjoin(fields.type, ', '), $
                       format='(%"insert into comp_cal (%s) values (%s)")')
      db->execute, sql_cmd, $
                   cal_basenames[f], $
                   date_obs, $
                   obs_day, $

                   wavelength, $
                   ntunes, $
                   level_id, $
                   exposure, $
                   ndfilter, $

                   cover, $
                   opal, $
                   polangle, $
                   polarizer, $
                   retarder, $

                   median_int_continuum, $
                   median_int_linecenter, $

                   occulter_id, $

                   status=status, $
                   error_message=error_message, $
                   sql_statement=final_sql_cmd, $
                   n_warnings=n_warnings
      if (status ne 0L) then begin
        mg_log, 'error inserting into comp_cal table', name='comp', /error
        mg_log, 'status: %d, error message: %s', status, error_message, $
                name='comp', /error
        mg_log, 'SQL command: %s', final_sql_cmd, name='comp', /error
      endif

      if (n_warnings gt 0L) then comp_db_log_warnings, database=db
    endfor
  endfor

  done:
  mg_log, 'done', name='comp', /info
end
