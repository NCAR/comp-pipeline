; docformat = 'rst'

;+
; Plot medians of flat.
;
; :Params:
;   flat_filename : in, required, type=string
;     filename of flat medians CSV file
;   dark_filename : in, required, type=string
;     filename of dark medians CSV file
;
; :Keywords:
;   start_date : in, optional, type=string
;     start date in the form "YYYYMMDD"
;-
pro comp_plot_flatmedians, flat_filename, dark_filename, $
                           yrange=yrange, $
                           start_date=start_date, $
                           constant_normalization=constant_normalization, $
                           output_filename=output_filename
  compile_opt strictarr

  normalization_factor = 84.0
  _yrange = n_elements(yrange) eq 0L ? [0.0, 70.0] : yrange

  if (n_elements(start_date) eq 0L) then begin
    jd_start_date = 0.0D
  endif else begin
    jd_start_date = julday(long(strmid(start_date, 4, 2)), $
                           long(strmid(start_date, 6, 2)), $
                           long(strmid(start_date, 0, 4)))
  endelse

  _output_filename = n_elements(output_filename) eq 0L $
                       ? 'flat-medians.ps' $
                       : output_filename

  ; read flats
  n_lines = file_lines(flat_filename)
  s = replicate({time: 0.0D, $
                 time_of_day: 0.0, $
                 wavelength: 0.0, $
                 median: 0.0}, n_lines)
  openr, lun, flat_filename, /get_lun
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

    if (keyword_set(constant_normalization)) then begin
      s[i].median *= comp_transmission(tokens[0]) / normalization_factor
    endif
  endfor
  free_lun, lun

  ; filter flats by dates
  valid_date_indices = where(s.time ge jd_start_date, n_valid_dates)
  if (n_valid_dates eq 0L) then message, 'no valid dates'
  s = s[valid_date_indices]

  ; read darks
  n_lines = file_lines(dark_filename)
  d = replicate({time: 0.0D, $
                 time_of_day: 0.0, $
                 median: 0.0}, n_lines)
  openr, lun, dark_filename, /get_lun
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

    d[i].time = jday
    d[i].time_of_day = float(tokens[1])
    d[i].median = float(tokens[2])
  endfor
  free_lun, lun

  ; filter darks by dates
  valid_date_indices = where(d.time ge jd_start_date, n_valid_dates)
  if (n_valid_dates eq 0L) then message, 'no valid dates for darks'
  d = d[valid_date_indices]

  eps = 0.01
  ind_1074 = where(abs(s.wavelength - 1074.62) lt eps, n_1074)
  ind_1083 = where(abs(s.wavelength - 1083.00) lt eps, n_1083)

  coeffs = poly_fit(s[ind_1074].time, s[ind_1074].median, 1)

  regress_line_1074 = coeffs[0] + coeffs[1] * s[ind_1074].time
  tolerance = 15.0
  bad_1074 = where(abs(regress_line_1074 - s[ind_1074].median) gt tolerance, n_bad_1074)

  print, 'Flat median outliers'
  for b = 0L, n_bad_1074 - 1L do begin
    print, s[ind_1074[bad_1074[b]]].time, s[ind_1074[bad_1074[b]]].median, $
           format='(C(), %" median: %0.1f")'
  endfor

  ;y_range = [0.0, 1.05 * max(s.median)]
  ;y_range = [0.0, 66.0]
  y_range = keyword_set(constant_normalization) ? _yrange : [0.0, 140.0]
  print, y_range, format='(%"flat range: %0.1f - %0.1f")'

  !null = label_date(date_format=['%M %D', '%Y'])

  mg_psbegin, filename=_output_filename, xsize=10.0, ysize=8.0, /inches, $
              /color, /landscape, xoffset=0.0

  device, decomposed=1
  plot, [s[0].time, s[-1].time], y_range, /nodata, $
        xticklen=-0.01, yticklen=-0.01, $
        xstyle=9, ystyle=9, $
        xtickformat=['LABEL_DATE', 'LABEL_DATE'], $
        xtickunits=['Time', 'Time'], $
        xminor=12, $
        xticks=12, $
        charsize=0.8, font=1, $
        title=string(keyword_set(constant_normalization) $
                       ? string(normalization_factor, format='(%"%0.1f")') $
                       : 'linear regression of flat tests', $
                     format='(%"Median flat values (normalized by %s)!C!5Medians normalized for solar distance (1 AU) and exposure time (250.0 ms)!X")'), $
        xtitle='Date', ytitle='Median values in both annuli', $
        position=[0.05, 0.55, 0.975, 0.95]
  axis, s[-1].time, 0.0, /yaxis, $
        charsize=0.8, font=1, $
        yticklen=-0.01, $
        yrange=y_range, ystyle=1

  annotation_charsize = 0.5

  ; cleanings
  cleanings = [julday(8, 11, 2014, 12), $
               julday(11, 21, 2014, 12), $
               julday(1, 23, 2015, 12), $
               julday(5, 22, 2015, 12), $
               julday(9, 16, 2015, 12), $
               julday(5, 4, 2016, 12), $
               julday(6, 25, 2016, 12), $
               julday(8, 16, 2016), $
               julday(11, 28, 2016), $
               julday(3, 24, 2017), $
               ;julday(3, 27, 2017), $  ; seems like this is only KCor cleaning
               julday(7, 14, 2017), $
               julday(1, 29, 2018), $
               julday(2, 26, 2018), $
               julday(2, 28, 2018), $
               julday(3, 16, 2018), $
               julday(3, 28, 2018)]
  valid_cleaning_indices = where(cleanings ge jd_start_date, /null)
  cleanings = cleanings[valid_cleaning_indices]
  for c = 0L, n_elements(cleanings) - 1L do begin
    oplot, fltarr(2) + cleanings[c], y_range, color='a0a0a0'x, thick=1.0
    xyouts, cleanings[c] + 10.0, y_range[1], $
            string(cleanings[c], $
                   format='(C(CYI, ".", CMOI2.2, ".", CDI2.2), " 01 cleaning")'), $
            orientation=90.0, charsize=annotation_charsize, font=1, $
            alignment=1.0
  endfor

  ; annotations
  annotation_dates = [julday(8, 27, 2013), $
                      julday(1, 15, 2014, 12), $
                      julday(4, 29, 2014, 12), $
                      julday(10, 19, 2014, 12), $
                      julday(12, 17, 2014, 12)]
  valid_annotation_indices = where(annotation_dates ge jd_start_date, /null)
  annotation_dates = annotation_dates[valid_annotation_indices]
  annotations = ['CoMP!Crestarted after!CMK4/CHIP removed', $
                 'CoMP warmed', $
                 'turned off!Ccamerato conserve LN2', $
                 'turned off!Ccamera to conserve LN2', $
                 'camera!Cwarmed/re-cooled']
  annotations = annotations[valid_annotation_indices]
  for c = 0L, n_elements(annotations) - 1L do begin
    oplot, fltarr(2) + annotation_dates[c], y_range, color='a0a0a0'x, thick=1.0
    xyouts, annotation_dates[c] + 10.0, y_range[1], $
            string(annotation_dates[c], annotations[c], $
                   format='(C(CYI, ".", CMOI2.2, ".", CDI2.2), %" %s")'), $
            orientation=90.0, charsize=annotation_charsize, font=1, $
            alignment=1.0
  endfor

  ; marks
  marks_dates = [julday(12, 8, 2012, 12), $
                 julday(5, 16, 2014, 12), $
                 julday(12, 17, 2014, 12), $
                 julday(3, 15, 2015, 12)]
  marks_heights = [53.0, 47.0, 40.0, 38.0]
  valid_mark_indices = where(marks_dates ge jd_start_date, /null)
  marks_dates = marks_dates[valid_mark_indices]
  marks_heights = marks_heights[valid_mark_indices]
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
         color='000000'x
  xyouts, julday(1, 1, 2013), 60.0, string(coeffs[1], format='(%"slope %0.6f")'), $
          /data, alignment=0.0, charsize=0.65, font=1

  flat_test_date = julday(12, 14, 2016)
  flat_test_value = 25.8
  xyouts, flat_test_date + 14.0, 31.0, 'Flat test', $
          /data, alignment=0.0, charsize=0.65, font=1
  t = 0.25
  plots, [14.0 * t + flat_test_date, flat_test_date + 14.0], $
         [(30.5 - flat_test_value) * t + flat_test_value, 30.5], $
         color='a0a0a0'x
  oplot, [flat_test_date], [flat_test_value], $
         color='000000'x, psym=mg_usersym(/circle, /fill), symsize=0.75

  plots, s[ind_1074].time, $
         coeffs[1] * (s[ind_1074].time - flat_test_date) + flat_test_value, $
         linestyle=1

  flat_test_date = julday(7, 21, 2017)
  flat_test_value = (21.38 + 21.55 + 22.00 + 21.62) / 4.0
  flat_test_date_offset = -35.0
  xyouts, flat_test_date + flat_test_date_offset, 39.0, 'Flat test', $
          /data, alignment=1.0, charsize=0.65, font=1
  t = 0.05
  plots, [flat_test_date_offset * t + flat_test_date, flat_test_date + flat_test_date_offset], $
         [(38.5 - flat_test_value) * t + flat_test_value, 38.5], $
         color='a0a0a0'x
  oplot, [flat_test_date], [flat_test_value], $
         color='000000'x, psym=mg_usersym(/circle, /fill), symsize=0.75
  
  plots, s[ind_1074].time, $
         coeffs[1] * (s[ind_1074].time - flat_test_date) + flat_test_value, $
         linestyle=1

  flat_test_date = julday(4, 4, 2018)
  flat_test_value = (22.82 + 22.63 + 22.59 + 22.61) / 4.0
  flat_test_date_offset = -75.0
  xyouts, flat_test_date + flat_test_date_offset, 39.0, 'Flat test', $
          /data, alignment=1.0, charsize=0.65, font=1
  t = 0.05
  plots, [flat_test_date_offset * t + flat_test_date, flat_test_date + flat_test_date_offset], $
         [(38.5 - flat_test_value) * t + flat_test_value, 38.5], $
         color='a0a0a0'x
  oplot, [flat_test_date], [flat_test_value], $
         color='000000'x, psym=mg_usersym(/circle, /fill), symsize=0.75
  
  plots, s[ind_1074].time, $
         coeffs[1] * (s[ind_1074].time - flat_test_date) + flat_test_value, $
         linestyle=1

  bad_morning_1074 = where(s[ind_1074[bad_1074]].time_of_day lt 9, n_bad_morning_1074, $
                       complement=bad_after_1074, ncomplement=n_bad_after_1074)

  if (n_bad_morning_1074 gt 0L) then begin
    oplot, [s[ind_1074[bad_1074[bad_morning_1074]]].time], $
           [s[ind_1074[bad_1074[bad_morning_1074]]].median], $
           color='00a5ff'x, psym=6, symsize=1.0
  endif

  if (n_bad_after_1074 gt 0L) then begin
    oplot, s[ind_1074[bad_1074[bad_after_1074]]].time, $
           s[ind_1074[bad_1074[bad_after_1074]]].median, $
           color='0000ff'x, psym=6, symsize=1.0
  endif

  filled_square = mg_usersym(/square, /fill)
  cgLegend, symColors=['00a5ff'x, '0000ff'x, '00ff00'x, 'ff0000'x], $
            psyms=lonarr(4) + filled_square, $
            symsize=1.0, $
            location=[0.80, 1.025], $
            titles=['1074.62 before 9 am HST', '1074.62 after 9 am HST', $
                    '1083.0 before 9 am HST', '1083.0 after 9 am HST'], $
            charsize=0.85, tt_font='Helvetica', /hardware, $
            vspace=1.1, $
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

  ;temp_threshold = -170.0
  ;bad_temps = where(temps lt temp_threshold, n_bad_temps)
  ;if (n_bad_temps gt 0L) then begin
  ;  temps[bad_temps] = !values.f_nan
  ;endif

  zero_temps = where(temps eq 0.0, n_zero_temps)
  if (n_zero_temps gt 0L) then begin
    temps[zero_temps] = !values.f_nan
  endif

  good_temps = where(finite(temps), n_good_temps)
  if (n_good_temps gt 0L) then begin
    temps = temps[good_temps]
    temp_times = temp_times[good_temps]
  endif

  dark_coeffs = poly_fit(d.time, d.median, 1)
  ;dark_range = [min(d.median, max=max_dark), max_dark]
  dark_range = [1800.0, 2050.0]
  print, dark_range, format='(%"dark range: %0.1f -- %0.1f")'
  plot, [s[0].time, s[-1].time], dark_range, /nodata, /noerase, $
        xstyle=9, ystyle=9, $
        yticklen=-0.01, $
        xtickformat=['LABEL_DATE', 'LABEL_DATE'], $
        xtickunits=['Time', 'Time'], $
        xminor=12, $
        xticks=12, $
        charsize=0.7, font=1, $
        title='Median dark values', $
        xtitle='Date', ytitle='Median values in both annuli', $
        position=[0.05, 0.3, 0.975, 0.425]
  oplot, d.time, d.median, psym=3, color='a0a0a0'x
  oplot, d.time, dark_coeffs[0] + dark_coeffs[1] * d.time, color='000000'x
  xyouts, (d.time)[-1], 2100.0, string(dark_coeffs[1], format='(%"slope %0.6f")'), $
          /data, alignment=1.0, charsize=0.65, font=1

  ;temp_range = [min(temps, max=max_temp), max_temp]
  temp_range = [-190.0, -170.0]
  print, temp_range, format='(%"temp range: %0.1f -- %0.1f")'
  temp_coeffs = poly_fit(temp_times, temps, 1)
  plot, [s[0].time, s[-1].time], temp_range, /nodata, /noerase, $
        xstyle=9, ystyle=9, $
        yticks=4, $
        yticklen=-0.01, $
        xtickformat=['LABEL_DATE', 'LABEL_DATE'], $
        xtickunits=['Time', 'Time'], $
        xminor=12, $
        xticks=12, $
        charsize=0.7, font=1, $
        title='Rockwell temperatures', $
        xtitle='Date', ytitle='Temperature (deg C)', $
        position=[0.05, 0.125, 0.975, 0.2]
  oplot, temp_times, temps, psym=3, color='a0a0a0'x
  oplot, temp_times, temp_coeffs[0] + temp_coeffs[1] * temp_times, color='000000'x
  xyouts, s[-1].time, -180.0, string(temp_coeffs[1], format='(%"slope %0.6f")'), $
          /data, alignment=1.0, charsize=0.65, font=1

  mg_psend
  print, _output_filename, format='(%"wrote %s")'
end


comp_plot_flatmedians, 'flat-medians.csv', 'dark-medians.csv', $
                       yrange=[0.0, 70.0], $
                       /constant_normalization, $
                       output_filename='flat-medians.ps'
 
comp_plot_flatmedians, 'flat-medians.csv', 'dark-medians.csv', $
                       yrange=[0.0, 40.0], $
                       /constant_normalization, $
                       start_date='20160101', $
                       output_filename='flat-medians-2016-2018.ps'

end
