; docformat = 'rst'

;+
; Determine transmission for a given date.
;
; :Returns:
;   float
;
; :Params:
;   date : in, required, type=string
;     date in the form YYYYMMDD
;
; :Author:
;   MLSO Software Team
;-
function comp_transmission, date
  compile_opt strictarr

  date_tokens = long(comp_decompose_date(date))
  jd = julday(date_tokens[1], date_tokens[2], date_tokens[0], 0, 0, 0)

  date1 = julday(6, 22, 2005, 0, 0, 0)
  value1 = 84.0

  date2 = julday(12, 14, 2016, 0, 0, 0)
  value2 = 25.8

  slope = (value2 - value1) / (date2 - date1)

  return, slope * (jd - date2) + value2
end
