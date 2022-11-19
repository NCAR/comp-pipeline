; docformat = 'rst'


pro comp_plot_sigma, filename
  compile_opt strictarr

  n_lines = file_lines(filename)
  data = dblarr(3, n_lines)

  openr, lun, filename, /get_lun
  readf, lun, data
  free_lun, lun

  datetimes = data[0, *]
  sigma = data[2, *]

  date_fmt = '(C(CYI04, CMOI02, CDI02))'
  dates = strarr(n_lines)
  for d = 0L, n_lines - 1L do begin
    dates[d] = string(datetimes[d], format=date_fmt)
  endfor

  high_sigma = 3.0
  very_high_sigma = 10.0

  high_sigma_indices = where(sigma gt high_sigma, n_high_sigma)
  very_high_sigma_indices = where(sigma gt very_high_sigma, n_very_high_sigma)

  very_high_sigma_dates = dates[very_high_sigma_indices]
  unique_very_high_sigma_dates = very_high_sigma_dates[uniq(very_high_sigma_dates, sort(very_high_sigma_dates))]

  for d = 0L, n_elements(unique_very_high_sigma_dates) - 1L do begin
    !null = where(very_high_sigma_dates eq unique_very_high_sigma_dates[d], count)
    print, unique_very_high_sigma_dates[d], count, format='(%"%s: %d files")'
  endfor
end


; main-level example program

f = 'comp.1074.bkg.sigma.txt'
comp_plot_sigma, f

end
