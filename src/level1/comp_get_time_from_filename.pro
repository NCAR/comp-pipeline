; docformat = 'rst'

;+
; Get time in hours from a filename which begins of the form "YYYYMMDD.HHMMSS".
;
; :Returns:
;   float
;
; :Params:
;   filename : in, required, type=string
;     filename
;
; :Author:
;   MLSO Software Team
;-
function comp_get_time_from_filename, filename
  compile_opt strictarr

  hours = strmid(filename, 9, 2)
  mins  = strmid(filename, 11, 2)
  secs  = strmid(filename, 13, 2)

  time = float(hours) + float(mins) / 60. + float(secs) / 3600.

  return, time
end
