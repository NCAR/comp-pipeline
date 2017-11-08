; docformat = 'rst'

;+
; Generate a string containing today's date.
;
; :Author:
;   MLSO Software Team
;-
function datecal
  compile_opt strictarr

  month_name = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', $
                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
  today = systime(/utc)
  month = strmid(today, 4, 3)
  month = where(month eq month_name, count) + 1
  if (count eq 0) then begin
    mg_log, 'month string wrong', name='comp', /warn
  endif

  month = month[0]
  month = string(month)
  month = strmid(month, 10, 2)
  day = strmid(today, 8, 2)
  year = strmid(today, 20, 4)
  datecal = string(year, month, day, format='(%"%s-%s-%s")')

  ; change spaces to zeros
  for n = 0L, strlen(datecal) do begin
    char = strmid(datecal, n, 1L)
    if (char eq ' ') then strput, datecal, '0', n
  endfor

  return, datecal
end