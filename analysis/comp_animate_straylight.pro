; docformat = 'rst'

pro comp_animate_straylight, eng_dir
  compile_opt strictarr

  files = file_search(filepath('*.ext-*.stray_light_fit.sav', root=eng_dir), $
                      count=n_files)

  fit_min = fltarr(n_files)
  fit_max = fltarr(n_files)

  print, n_files, format='(%"%d files")'

  for f = 0L, n_files - 1L do begin
    if (f mod 500 eq 0) then print, f + 1, n_files, format='(%"%d/%d: scanning")'
    restore, files[f]
    fit_min[f] = min(fit, max=fmax)
    fit_max[f] = fmax
  endfor

  fit_min = min(fit_min)
  fit_max = max(fit_max)

  print, fit_min, fit_max, format='(%"min: %0.3f, max: %0.3f")'

  device, decomposed=0
  loadct, 39, /silent
  tvlct, r, g, b, /get

  for f = 0L, n_files - 1L do begin
    if (f mod 500 eq 0) then print, f + 1, n_files, format='(%"%d/%d: writing")'
    restore, files[f]
    write_png, string(f, format='(%"%05d.png")'), $
               bytscl(fit, min=fit_min, max=fit_max), $
               r, g, b
  endfor
end


; main-level example program

eng_dir = '/hao/sunset/Data/CoMP/engineering.olddist-plainstray/2013/01/06'
comp_animate_straylight, eng_dir

end
