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
    s[i].time = jday
    s[i].wavelength = float(tokens[2])
    s[i].median = float(tokens[3])
  endfor
  free_lun, lun

  eps = 0.01
  ind_1074 = where(abs(s.wavelength - 1074.62) lt eps, n_1074)
  ind_1083 = where(abs(s.wavelength - 1083.00) lt eps, n_1083)
  
  !null = label_date(date_format='%Y %M %D')

  mg_psbegin, filename='flat-medians.ps', xsize=6, ysize=4, /inches, /color
  device, decomposed=1
  plot, [s[0].time, s[-1].time], [0.0, 0.6], /nodata, $
        xstyle=9, ystyle=9, xtickformat='LABEL_DATE', charsize=0.8, $
        title='Flat medians', xtitle='Date', ytitle='Median values in annuli'
  oplot, s[ind_1074].time, s[ind_1074].median, $
         color='0000ff'x, psym=1, symsize=0.5         
  oplot, s[ind_1083].time, s[ind_1083].median, $
         color='ff0000'x, psym=2, symsize=0.5
  cgLegend, symColors=['0000ff'x, 'ff0000'x], $
            psyms=[1, 2], $
            symsize=0.5, $
            location=[0.8, 0.9], $
            titles=['1074.62', '1083.0'], $
            charsize=0.8, $
            length=0.0
  mg_psend
end


comp_plot_flatmedian, 'flat-medians.csv'

end