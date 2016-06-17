; docformat = 'rst'

pro comp_plot_flatmedian, filename
  compile_opt strictarr

  n_lines = file_lines(filename)
  s = replicate({time: 0.0, wavelength: 0.0, median: 0.0}, n_lines)
  data = fltarr(4, n_lines)
  openr, lun, filename, /get_lun
  line = ''
  for i = 0L, n_lines - 1L do begin
    readf, lun, line
    tokens = strsplit(line, ',', /extract)

    jday = julday(strmid(tokens[0], 4, 2), $
                  strmid(tokens[0], 6, 2), $
                  strmid(tokens[0], 0, 4), $
                  tokens[1])
    s[i].time = jday ;(jday - julday(1, 1, 1970, 0, 0, 0)) * (24.0D * 60.0D * 60.0D)
    s[i].wavelength = float(tokens[2])
    s[i].median = float(tokens[3])
  endfor
  free_lun, lun

  eps = 0.01
  ind_1074 = where(abs(s.wavelength - 1074.62) lt eps, n_1074)
  ind_1083 = where(abs(s.wavelength - 1083.00) lt eps, n_1083)
  
  !null = label_date(date_format='%Y %M %D')

  plot, [s[0].time, s[-1].time], [0.0, 1.0], /nodata, $
        xstyle=9, ystyle=9, xtickformat='LABEL_DATE'
  oplot, s[ind_1074].time, s[ind_1074].median, $
         color='ff00ff'x
  oplot, s[ind_1083].time, s[ind_1083].median, $
         color='00ffff'x
end


comp_plot_flatmedian, 'flat-medians.csv'

end