; docformat = 'rst'

pro comp_analyze_reprocess_bkgs, filename
  compile_opt strictarr

  n_lines = file_lines(filename)
  data = dblarr(3, n_lines)
  openr, lun, filename, /get_lun
  readf, lun, data
  free_lun, lun

  jds = reform(data[0, *])
  sigma = reform(data[2, *])

  caldat, jds, month, day, year, hour, minute, second
  unique_years = year[uniq(year, sort(year))]

  sigma_threshold = [1.10, 1.15, 1.20, 1.25, 1.30, 1.35]

  for s = 0L, n_elements(sigma_threshold) - 1L do begin
    for y = 0L, n_elements(unique_years) - 1L do begin
      year_indices = where(year eq unique_years[y], n_files_per_year)
      print, unique_years[y], sigma_threshold[s], $
             format='Year: %d [sigma theshold: %0.2f]'
      !null = where(sigma[year_indices] gt sigma_threshold[s], n_bad_sigma_per_year)
      print, n_bad_sigma_per_year, n_files_per_year, $
             100.0 * n_bad_sigma_per_year / n_files_per_year, $
             format='%d / %d files with bad sigma (%0.1f%%)'
    endfor
  endfor
end


; main-level example

filename = 'comp.1074.bkg.sigma.txt'

comp_analyze_reprocess_bkgs, filename

end

