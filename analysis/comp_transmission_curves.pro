; docformat = 'rst'

; using 2nd order polynomial fit of three diffuser transmission test values,
; the 2018-04-04 value was anomalous and omitted from the fit
;
; transmission_test_dates = [julday(6, 22, 2005), $
;  julday(12, 14, 2016), $
;  julday(7, 21, 2017)];, $
;  ;julday(4, 4, 2018)]
; transmission_test_values = [84.0D, $
;   25.8D, 21.6D];, 22.7D]

pro comp_transmission_curves
  compile_opt strictarr

  date0 = julday(6, 22, 2005)
  date1 = julday(12, 14, 2016)
  date2 = julday(7, 21, 2017)
  transmission_test_dates = [date0, date1, date2]
  transmission_test_dates -= date0
  transmission_test_values = [84.0D, 25.8D, 21.6D]

  linear_coeffs = poly_fit(transmission_test_dates, transmission_test_values, 1)
  quadratic_coeffs = poly_fit(transmission_test_dates, transmission_test_values, 2)

  jds = timegen(start=julday(1, 1, 2012), final=julday(4, 4, 2018), step_size=1.0D)

  linear_values = poly(jds - date0, linear_coeffs)
  quadratic_values = poly(jds - date0, quadratic_coeffs)

  !null = label_date(date_format='%Y-%N')

  window, xsize=800, ysize=400, /free, title='linear vs quadratic'
  plot, jds, linear_values, /nodata, xtickformat='label_date'
  oplot, jds, linear_values
  oplot, jds, quadratic_values
  xyouts, jds[-1] + 20.0, linear_values[-1], 'linear', /data, charsize=0.9
  xyouts, jds[-1] + 20.0, quadratic_values[-1], 'quadratic', /data, charsize=0.9

  window, xsize=800, ysize=400, /free, title='difference (quadratic_values - linear_values)'
  difference = quadratic_values - linear_values
  plot, jds, difference, xtickformat='label_date'
end


; main-level example program

comp_transmission_curves

end
