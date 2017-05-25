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
;-
function comp_transmission, date
  compile_opt strictarr

  date_tokens = long(comp_decompose_date(date))
  jd = julday(date_tokens[1], date_tokens[2], date_tokens[0], 0, 0, 0)

  regression_slope = -0.019652

  flat_test_date = julday(12, 14, 2016, 0, 0, 0)
  flat_test_value = 25.8

  return, regression_slope * (jd - flat_test_date) + flat_test_value
end
