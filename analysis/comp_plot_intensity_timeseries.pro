; docformat = 'rst'

pro comp_plot_intensity_timeseries, filename, background=background
  compile_opt strictarr

  print, file_basename(filename), format='plotting %s...'

  heights = [1.08, 1.10, 1.15, 1.20, 1.25]

  n_lines = file_lines(filename)
  openr, lun, filename, /get_lun
  data = dblarr(n_elements(heights) + 1L, n_lines)
  readf, lun, data
  free_lun, lun

  jds = reform(data[0, *])
  window, xsize=1000, ysize=1200, title=filename, /free
  !null = label_date(date_format='%Y-%N')
  !p.multi = [0, 1, n_elements(heights)]
  for h = 0L, n_elements(heights) - 1L do begin
    plot, jds, reform(data[h + 1L, *]), $
          title=string(keyword_set(background) ? 'Background' : 'Intensity', $
                       heights[h], $
                       format='%s at %0.2f Rsun'), $
          xstyle=1, xtickformat='label_date', xticks=10, $
          ystyle=1, yrange=[0.0, keyword_set(background) ? 50.0 : 7.0], $
          psym=6, symsize=0.25, $
          charsize=2.0
  endfor
  !p.multi = 0
end


; main-level example program

comp_plot_intensity_timeseries, '1074-mean-intensities.txt'
comp_plot_intensity_timeseries, '1074-median-intensities.txt'
comp_plot_intensity_timeseries, '1079-mean-intensities.txt'
comp_plot_intensity_timeseries, '1079-median-intensities.txt'
; comp_plot_intensity_timeseries, '1074-backgrounds.txt', /background
; comp_plot_intensity_timeseries, '1079-backgrounds.txt', /background

end
