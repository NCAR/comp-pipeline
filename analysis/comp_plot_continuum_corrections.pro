; docformat = 'rst'

pro comp_plot_continuum_corrections, filename
  compile_opt strictarr

  n_lines = file_lines(filename)

  data = strarr(n_lines)
  openr, lun, filename, /get_lun
  readf, lun, data
  free_lun, lun

  dates = strarr(n_lines)
  hours = fltarr(n_lines)
  offsets = fltarr(2, n_lines)
  for i = 0L, n_lines - 1L do begin
    tokens = strsplit(data[i], /extract)
    dates[i] = tokens[0]
    hours[i] = float(tokens[1])
    offsets[*, i] = float(tokens[2:3])
  endfor

  years = long(strmid(dates, 0, 4))
  months = long(strmid(dates, 4, 2))
  days = long(strmid(dates, 6, 2))
  jds = julday(months, days, years, hours)

  !null = label_date(date_format='%Y-%N')

  xsize = 1200
  ysize = 400
  colors = ['0000ff'x, '00ff00'x]
  psyms = [4, 5]
  charsize = 1.1

  title = string(file_basename(filename), format='(%"Offsets for %s")')
  window, xsize=xsize, ysize=ysize, /free, title=title
  plot, jds, reform(offsets[0, *]), /nodata, $
        color='000000'x, background='ffffff'x, $
        xstyle=1, xtickformat='label_date', xtitle='dates', $
        ystyle=1, yrange=[0.03, 0.045], ytitle='offset', $
        title=title, charsize=charsize
  xyouts, 0.1, 0.9, 'red=beam 1!Cgreen=beam 2', /normal, $
          color='000000'x, charsize=charsize
  for b = 0, 1 do begin
    oplot, jds[b:*:2], offsets[0, b:*:2], $
           psym=psyms[b], symsize=0.75, color=colors[b]
  endfor

  title = string(file_basename(filename), format='(%"H!D2!NO for %s")')
  window, xsize=xsize, ysize=ysize, /free, title=title
  plot, jds, reform(offsets[1, *]), /nodata, $
        color='000000'x, background='ffffff'x, $
        xstyle=1, xtickformat='label_date', xtitle='dates', $
        ystyle=1, yrange=[-0.25, 2.5], ytitle='offset', $
        title=title, charsize=charsize
  xyouts, 0.1, 0.9, 'red=beam 1!Cgreen=beam 2', /normal, $
          color='000000'x, charsize=charsize
  for b = 0L, 1L do begin
    oplot, jds[b:*:2], offsets[1, b:*:2], $
           psym=psyms[b], symsize=0.75, color=colors[b]
  endfor
end


; main-level example program

;eng_basedir = '/hao/twilight/Data/CoMP/engineering.continuum-correction-2014'
;basename = 'wave_cal_1074-2014.txt'

;eng_basedir = '/hao/twilight/Data/CoMP/engineering.continuum-correction-2017'
;basename = 'wave_cal_1074-2017.txt'

eng_basedir = '/hao/twilight/Data/CoMP/engineering.continuum-correction-2017-destray'
basename = 'wave_cal_1074-2017-destray.txt'

filename = filepath(basename, root=eng_basedir)
comp_plot_continuum_corrections, filename

end
