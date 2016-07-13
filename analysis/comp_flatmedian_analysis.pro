; docformat = 'rst'


pro comp_flatmedian_analysis_lc, line_centers, fcb, date, times, lun
  compile_opt strictarr

  ; setup erode structure for making sure mask is only getting annulus values
  erode_s = bytarr(25, 25) + 1B

  for lc = 0L, n_elements(line_centers) - 1L do begin
    fits_read, fcb, flat_data, flat_header, exten_no=line_centers[lc] + 1L

    mask = comp_annulus_1024(flat_header, o_offset=0.0, f_offset=0.0)
    mask = erode(mask, erode_s)
    mask_ind = where(mask, n_mask_values)
    flat_median = median(flat_data[mask_ind])

    exposure = sxpar(flat_header, 'EXPOSURE')
    flat_median *= 250.0 / exposure

    wavelength = sxpar(flat_header, 'WAVELENG')
    printf, lun, date, times[line_centers[lc]], wavelength, flat_median, $
            format='(%"%s, %f, %0.2f, %f")'
  endfor
end


pro comp_flatmedian_analysis, process_dir, output_filename
  compile_opt strictarr

  after = '20151101'
  openw, lun, output_filename, /get_lun

  ; loop over date dirs in process_dir
  dirs = file_search(filepath('*', root=process_dir), $
                     /test_directory, $
                     count=n_dirs)
  for d = 0L, n_dirs - 1L do begin
    date = file_basename(dirs[d])

    if (date eq '20141016') then continue
    if (date eq '20160701') then continue

    if (~stregex(date, '^[[:digit:]]{8}$', /boolean) || date lt after) then begin
      mg_log, 'skipping %s', date
      continue
    endif else begin
      mg_log, 'calculating %s', date
    endelse

    catch, error
    if (error ne 0) then begin
      print, date, format='(%"Error on %s, skipping")'
      continue
    endif

    ; find flat.fts file
    flat_filename = filepath('flat.fts', root=dirs[d])
    if (~file_test(flat_filename)) then begin
      flat_filename = filepath('flat.fts', subdir='level1', root=dirs[d])
    endif
    if (~file_test(flat_filename)) then continue

    fits_open, flat_filename, flat_fcb

    fits_read, flat_fcb, wavelengths, wavelengths_header, $
               exten_no=flat_fcb.nextend - 1L
    fits_read, flat_fcb, times, times_header, $
               exten_no=flat_fcb.nextend - 2L

    eps = 0.01

    line_centers_1074 = where(abs(wavelengths - 1083.00) lt eps, n_line_centers_1074)
    if (n_line_centers_1074 gt 0) then begin
      comp_flatmedian_analysis_lc, line_centers_1074, flat_fcb, date, times, lun
    endif

    line_centers_1083 = where(abs(wavelengths - 1074.62) lt eps, n_line_centers_1083)
    if (n_line_centers_1083 gt 0) then begin
      comp_flatmedian_analysis_lc, line_centers_1083, flat_fcb, date, times, lun
    endif

    fits_close, flat_fcb
    flush, lun
  endfor

  free_lun, lun
end


process_dir = '/hao/kaula1/Data/CoMP/process'
comp_flatmedian_analysis, process_dir, 'flat-medians-new.csv'

end
