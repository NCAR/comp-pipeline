; docformat = 'rst'

;+
; Plot medians of flat.
;
; :Params:
;   filename : in, required, type=string
;     filename of flat medians CSV file
;-
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

  print, 'Flat median outliers'
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

  mg_psbegin, filename='flat-medians.ps', xsize=10.0, ysize=8.0, /inches, $
              /color, /landscape, xoffset=0.0

  device, decomposed=1
  plot, [s[0].time, s[-1].time], y_range, /nodata, $
        xstyle=9, ystyle=9, $
        xtickformat=['LABEL_DATE', 'LABEL_DATE'], $
        xtickunits=['Time', 'Time'], $
        xminor=12, $
        xticks=12, $
        charsize=0.8, font=1, $
        title='Median flat values!CMedians normalized for solar distance (1 AU) and exposure time (250.0 ms)', $
        xtitle='Date', ytitle='Median values in both annuli', $
        position=[0.05, 0.45, 0.975, 0.9]

  annotation_charsize = 0.5

  ; cleanings
  for c = 0L, n_elements(cleanings) - 1L do begin
    oplot, fltarr(2) + cleanings[c], y_range, color='a0a0a0'x, thick=1.0
    xyouts, cleanings[c] + 10.0, y_range[1], $
            string(cleanings[c], $
                   format='(C(CYI, ".", CMOI2.2, ".", CDI2.2), " 01 cleaning")'), $
            orientation=90.0, charsize=annotation_charsize, font=1, $
            alignment=1.0
  endfor

  ; annotations
  annotation_dates = [julday(1, 15, 2014, 12), $
                      julday(4, 29, 2014, 12), $
                      julday(10, 19, 2014, 12), $
                      julday(12, 17, 2014, 12)]
  annotations = ['CoMP warmed', $
                 'turned off!Ccamerato conserve LN2', $
                 'turned off!Ccamera to conserve LN2', $
                 'camera!Cwarmed/re-cooled']

  for c = 0L, n_elements(annotations) - 1L do begin
    oplot, fltarr(2) + annotation_dates[c], y_range, color='a0a0a0'x, thick=1.0
    xyouts, annotation_dates[c] + 10.0, y_range[1], $
            string(annotation_dates[c], annotations[c], $
                   format='(C(CYI, ".", CMOI2.2, ".", CDI2.2), %" %s")'), $
            orientation=90.0, charsize=annotation_charsize, font=1, $
            alignment=1.0
  endfor

  ; marks
  marks_dates = [julday(5, 16, 2014, 12), $
                 julday(12, 17, 2014, 12), $
                 julday(3, 15, 2015, 12)]
  marks_heights = [47.0, 40.0, 38.0]
  for a = 0L, n_elements(marks_dates) - 1L do begin
    oplot, fltarr(2) + marks_dates[a], [-1.5, -0.5] + marks_heights[a], $
           color='a0a0a0'x, thick=1.0
    xyouts, marks_dates[a], marks_heights[a], $
            string(marks_dates[a], format='(C(CYI, ".", CMOI2.2, ".", CDI2.2))'), $
            charsize=annotation_charsize, font=1
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
            location=[0.775, 0.9], $
            titles=['1074.62', '1074 morning', '1083.0', '1083.0 morning'], $
            charsize=0.85, tt_font='Helvetica', /hardware, $
            length=0.0

  temp_filename = 'rockwell-temp-record.txt'
  n_temps = file_lines(temp_filename)
  data = dblarr(2, n_temps)
  openr, lun, temp_filename, /get_lun
  readf, lun, data
  free_lun, lun

  temps = data[1, *]
  temp_times = data[0, *]

  start = julday(1, 1, 1904, 0)
  temp_times = start + temp_times / (24.0 * 60.0 * 60.0)

  temp_threshold = -170.0
  bad_temps = where(temps gt temp_threshold, n_bad_temps)
  if (n_bad_temps gt 0L) then begin
    temps[bad_temps] = !values.f_nan
  endif

  temp_range = [min(temps, max=max_temp), max_temp]

  plot, [s[0].time, s[-1].time], temp_range, /nodata, /noerase, $
        xstyle=9, ystyle=9, $
        xtickformat=['LABEL_DATE', 'LABEL_DATE'], $
        xtickunits=['Time', 'Time'], $
        xminor=12, $
        xticks=12, $
        charsize=0.8, font=1, $
        title='Rockwell temperatures', $
        xtitle='Date', ytitle='Temperature (deg C)', $
        position=[0.05, 0.15, 0.975, 0.3]

  oplot, temp_times, temps, psym=3, color='a0a0a0'x

  mg_psend
end


comp_plot_flatmedians, 'flat-medians.csv'

end