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

      ; TODO: insert into database
    endfor
  endfor
end
