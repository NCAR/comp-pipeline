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
  producttype_id = comp_get_producttype_id('pbcart', database=db)

  for f = 0L, n_l1_files - 1L do begin
    fits_open, l1_files[f], fcb
    fits_read, fcb, data, primary_header, exten_no=0, /no_abort, message=msg
    fits_close, fcb

    carrington_rotation = sxpar(primary_header, 'CARR_ROT')
    ntunes = sxpar(primary_header, 'NTUNES')

  endfor

  done:
  mg_log, 'done', name='comp', /info
end
