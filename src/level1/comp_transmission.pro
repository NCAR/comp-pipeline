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

  date0 = julday(6, 22, 2005, 0, 0, 0)

  ; using 2nd order polynomial fit of three diffuser transmission test values,
  ; the 2018-04-04 value was anomalous and omitted from the fit
  ;
  ; transmission_test_dates = [julday(6, 22, 2005), $
  ;  julday(12, 14, 2016), $
  ;  julday(7, 21, 2017)];, $
  ;  ;julday(4, 4, 2018)]
  ; transmission_test_values = [84.0D, $
  ;   25.8D, 21.6D];, 22.7D]
    
  coeffs = [84.00000000, -0.00884544, -0.00000120]

  return, poly(jd - date0, coeffs)
end


; main-level example program

dates = ['20130118', '20131026', '20140115', '20140119', '20140202', '20140209', '20141024', '20141027', '20141029', '20141116']
for d = 0L, n_elements(dates) - 1L do begin
  print, dates[d], comp_transmission(dates[d]), format='(%"%s: %0.2f")'
endfor

end
