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

end
