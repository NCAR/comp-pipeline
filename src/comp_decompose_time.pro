; docformat = 'rst'

;+
; Decompose a time string into year, month, day.
;
; :Returns:
;   `strarr(3)`
;
; :Params:
;   time : in, required, type=string
;     date in the form "143512" representing 2:35:12 pm
;
; :Author:
;   MLSO Software Team
;-
function comp_decompose_time, date
  compile_opt strictarr

  hrs  = strmid(date, 0, 2)
  mins = strmid(date, 2, 2)
  secs = strmid(date, 4, 2)

  return, [hrs, mins, secs]
end
