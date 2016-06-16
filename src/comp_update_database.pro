; docformat = 'rst'

;+
; Update database with results of pipeline run for the given day.
;
; :Todo:
;   implement
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;-
pro comp_update_database, date_dir, wave_type
  compile_opt strictarr
  @comp_config_common

end
