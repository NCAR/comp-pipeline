; docformat = 'rst'

pro comp_plot_straylight, filename
  compile_opt strictarr

  n_coeffs = 9

  x = read_csv(filename)

  filenames = x.field01
  wavelengths = x.field03
  polstates = x.field04

  unique_wavelengths = wavelengths[uniq(wavelengths, sort(wavelengths))]
  n_unique_wavelengths = n_elements(unique_wavelengths)

  n_files = n_elements(wavelengths)

  coeffs = dblarr(n_coeffs, n_files)

  !p.multi = [0, 3, 3]

  device, decomposed=0
  loadct, 49
  tvlct, 255, 255, 255, 0
  tvlct, 0, 0, 0, 255

  colors = bytscl(lindgen(n_unique_wavelengths))

  for c = 0, n_coeffs - 1L do begin
    coeffs[c, *] = x.(c + 4)
  endfor

  column_names = 'c' + strjoin(strtrim(array_indices(lonarr(3, 3), $
                                                     lindgen(n_coeffs)), $
                                       2), $
                               ',')
  t = mg_table(coeffs, column_names=column_names)
  s = t.stats()
  print, s

  ranges = [[90.0, 200.0], $
            [-1.0e-1, 1.0e-1], $
            [-5.0e-4, 2.0e-4], $
            [-1.0e-1, 5.0e-2], $
            [-2.0e-4, 0.0], $
            [-2.0e-7, 7.0e-7], $
            [-4.0e-4, 1e-4], $
            [0.0, 7.0e-7], $
            [-2.0e-9, 1.0e-9]]

  for c = 0, 8 do begin
    plot, findgen(n_files), coeffs[c, *], $
          /nodata, yrange=reform(ranges[*, c]), xstyle=9, ystyle=9, charsize=2.0, $
          title=column_names[c], xtitle='image index'

    for w = 0L, n_unique_wavelengths - 1L do begin
      ind = where(wavelengths eq unique_wavelengths[w], count)
      if (count eq 0) then continue
      x = ind
      y = reform(coeffs[c, ind])

      outlier_ind = where(y lt ranges[0, c] or y gt ranges[1, c], outlier_count)

      if (outlier_count gt 0L) then begin
        print, outlier_count, count, unique_wavelengths[w], column_names[c], $
               format='(%"\n# %d outliers out of %d images for %0.2f nm for %s\n")'
        outlier_t = mg_table({filenames: filenames[ind[outlier_ind]], $
                              pol: polstates[ind[outlier_ind]]}, column_widths=[19, 3], $
                             n_rows_to_print=n_files)
        print, outlier_t
        obj_destroy, outlier_t
      endif

      y = mg_insert_gap(x, y, min_gap_length=1.5, new_x=x)

      oplot, x, y, color=colors[w]
    endfor
  endfor

  obj_destroy, [t, s]

  !p.multi = 0
end


; main-level example program

date = '20160517'
config_filename = filepath('comp.mgalloy.compdata.ps2.cfg', $
                           subdir=['..', 'config'], $
                           root=mg_src_root())
config = mg_read_config(config_filename)
eng_basedir = config->get('engineering_dir', section='engineering')
obj_destroy, config

filename = filepath(string(date, format='(%"%s.comp.straylight.csv")'), $
                    subdir=comp_decompose_date(date), $
                    root=eng_basedir)

comp_plot_straylight, filename

end
