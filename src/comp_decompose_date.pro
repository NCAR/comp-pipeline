; docformat = 'rst'

;+
; Decompose a date string into year, month, day.
;
; :Returns:
;   `strarr(3)` or `strarr(n, 3)` where `n` is the number of date strings passed
;   in through `date`
;
; :Params:
;   date : in, required, type=string or strarr(n)
;     date(s) in the form "20150801"
;
; :Author:
;   MLSO Software Team
;-
function comp_decompose_date, date
  compile_opt strictarr

  year = strmid(date, 0, 4)
  month = strmid(date, 4, 2)
  day = strmid(date, 6, 2)

  return, reform([[year], [month], [day]])
end
