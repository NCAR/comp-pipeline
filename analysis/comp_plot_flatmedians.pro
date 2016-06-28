; docformat = 'rst'

pro comp_plot_flatmedians, filename
  compile_opt strictarr

  n_lines = file_lines(filename)
  s = replicate({time: 0.0D, $
                 time_of_day: 0.0, $
                 wavelength: 0.0, $
                 median: 0.0}, n_lines)
  data = fltarr(4, n_lines)
  openr, lun, filename, /get_lun
  line = ''
  for i = 0L, n_lines - 1L do begin
    readf, lun, line
    tokens = strsplit(line, ',', /extract)

    month = long(strmid(tokens[0], 4, 2))
    day = long(strmid(tokens[0], 6, 2))
    year = long(strmid(tokens[0], 0, 4))
    hour = long(tokens[1])
    minute = long((float(tokens[1]) - hour) * 60)
    second = long(((float(tokens[1]) - hour) * 60 - minute) * 60)
    jday = julday(month, day, year, hour, minute, second)

    sun, year, month, day, float(tokens[1]) + 10.0, dist=solar_distance

    s[i].time = jday
    s[i].time_of_day = float(tokens[1])
    s[i].wavelength = float(tokens[2])
    s[i].median = float(tokens[3]) * solar_distance^2
  endfor
  free_lun, lun

  eps = 0.01
  ind_1074 = where(abs(s.wavelength - 1074.62) lt eps, n_1074)
  ind_1083 = where(abs(s.wavelength - 1083.00) lt eps, n_1083)

  coeffs = poly_fit(s[ind_1074].time, s[ind_1074].median, 1)

  regress_line_1074 = coeffs[0] + coeffs[1] * s[ind_1074].time
  tolerance = 5.0
  bad_1074 = where(abs(regress_line_1074 - s[ind_1074].median) gt tolerance, n_bad_1074)

  for b = 0L, n_bad_1074 - 1L do begin
    print, s[ind_1074[bad_1074[b]]].time, s[ind_1074[bad_1074[b]]].median, $
           format='(C(), %" median: %0.1f")'
  endfor


  cleanings = [julday(8, 11, 2014, 12), $
               julday(11, 21, 2014, 12), $
               julday(1, 23, 2015, 12), $
               julday(5, 22, 2015, 12), $
               julday(9, 16, 2015, 12), $
               julday(5, 4, 2016, 12)]

  y_range = [0.0, 1.05 * max(s.median)]

  !null = label_date(date_format=['%M %D', '%Y'])

  mg_psbegin, filename='flat-medians.ps', xsize=9, ysize=6, /inches, $
              /color, /landscape

  device, decomposed=1
  plot, [s[0].time, s[-1].time], y_range, /nodata, $
        xstyle=9, ystyle=9, $
        xtickformat=['LABEL_DATE', 'LABEL_DATE'], $
        xtickunits=['Time', 'Time'], $
        xminor=12, $
        xticks=12, $
        charsize=0.8, font=1, $
        title='Flat values!CMedians corrected for solar distance and exposure time', $
        xtitle='Date', ytitle='Median values in annuli'

  ; cleanings
  for c = 0L, n_elements(cleanings) - 1L do begin
    oplot, fltarr(2) + cleanings[c], y_range, color='a0a0a0'x, thick=1.0
    xyouts, cleanings[c] + 10.0, 50.0, $
            string(cleanings[c], format='("Cleaning ", C(CYI, ".", CMOI0.2, ".", CDI0.2))'), $
            orientation=90.0, charsize=0.6, font=1
  endfor

  ; 1074 and mornings
  morning_1074 = where(s[ind_1074].time_of_day lt 9, n_morning_1074, $
                       complement=after_1074, ncomplement=n_after_1074)
  oplot, s[ind_1074[after_1074]].time, s[ind_1074[after_1074]].median, $
         color='0000ff'x, psym=1, symsize=0.5
  oplot, s[ind_1074[morning_1074]].time, s[ind_1074[morning_1074]].median, $
         color='00a5ff'x, psym=1, symsize=0.5

  ; 1083 and mornings
  morning_1083 = where(s[ind_1083].time_of_day lt 9, n_morning_1083, $
                       complement=after_1083, ncomplement=n_after_1083)
  oplot, s[ind_1083[after_1083]].time, s[ind_1083[after_1083]].median, $
         color='ff0000'x, psym=1, symsize=0.5
  oplot, s[ind_1083[morning_1083]].time, s[ind_1083[morning_1083]].median, $
         color='00ff00'x, psym=1, symsize=0.5

  ; regression line and outliers
  oplot, s[ind_1074].time, regress_line_1074, $
         color='0000ff'x
  oplot, s[ind_1074[bad_1074]].time, s[ind_1074[bad_1074]].median, $
         color='0000ff'x, psym=6, symsize=1.0

  cgLegend, symColors=['0000ff'x, '00a5ff'x, 'ff0000'x, '00ff00'x], $
            psyms=[1, 1, 2, 2], $
            symsize=0.5, $
            location=[0.8, 0.9], $
            titles=['1074.62', '1074 morning', '1083.0', '1083.0 morning'], $
            charsize=0.8, tt_font='Helvetica', $
            length=0.0
  mg_psend
end


comp_plot_flatmedians, 'flat-medians.csv'

end