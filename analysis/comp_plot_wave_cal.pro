; docformat = 'rst'

pro comp_plot_wave_cal, filename
  compile_opt strictarr

  n_lines = file_lines(filename)

  lines = strarr(n_lines)
  openr, lun, filename, /get_lun
  readf, lun, lines
  free_lun, lun

  data = replicate({date:'', time:0.0, jd:0.0D, offset:0.0, h2o:0.0}, n_lines)
  for i = 0L, n_lines - 1L do begin
    tokens = strsplit(lines[i], /extract, count=n_tokens)
    if (n_tokens ne 5) then message, string(i + 1, format='(%"problem on line %d")')
    data[i].date = tokens[0]
    data[i].time = float(tokens[1])
    data[i].offset = float(tokens[2])
    data[i].h2o = float(tokens[3])
  endfor

  years  = strmid(data.date, 0, 4)
  months = strmid(data.date, 4, 2)
  days   = strmid(data.date, 6, 2)

  jds = julday(months, days, years, data.time)

  !null = label_date(date_format='%M %Y')

  offset = data.offset

  bad_indices = where(data.offset lt 0.0, n_bad)
  if (n_bad gt 0L) then begin
    offset[bad_indices] = !values.f_nan
  endif

  window, xsize=900, ysize=400, /free, title=file_basename(filename)
  plot, jds, offset, psym=4, title='Offset', $
        xstyle=9, xtickformat='label_date', xtitle='date', xticks=12, $
        ystyle=9, yrange=[0.0, 0.1], ytitle='offset [nm]'

  smoothed_offset = smooth(offset, 29, /edge_truncate, /nan)
  oplot, jds, smoothed_offset, thick=2, color='00ffff'x

  window, xsize=900, ysize=400, /free, title=file_basename(filename)
  plot, jds, data.h2o, psym=4, title='H2O', $
        xstyle=9, xtickformat='label_date', xtitle='date', xticks=12, $
        ystyle=9, yrange=[0.0, 5.0], ytitle='offset [nm]'
end


; main-level example program

root = '/hao/twilight/Data/CoMP/engineering.continuum-correction-2017'
basename = 'wave_cal_1079-2017.txt'
filename = filepath(basename, root=root)

comp_plot_wave_cal, filename


end
