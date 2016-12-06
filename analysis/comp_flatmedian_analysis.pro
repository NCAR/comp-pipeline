; docformat = 'rst'


pro comp_flatmedian_analysis_lc, line_centers, flat_fcb, dark_fcb, $
                                 date, times, $
                                 flat_lun, dark_lun
  compile_opt strictarr

  ; setup erode structure for making sure mask is only getting annulus values
  erode_s = bytarr(25, 25) + 1B

  for lc = 0L, n_elements(line_centers) - 1L do begin
    fits_read, flat_fcb, flat_data, flat_header, exten_no=line_centers[lc] + 1L

    mask = comp_annulus_1024(flat_header, o_offset=0.0, f_offset=0.0)
    mask = erode(mask, erode_s)
    mask_ind = where(mask, n_mask_values)
    flat_median = median(flat_data[mask_ind])

    exposure = sxpar(flat_header, 'EXPOSURE')
    flat_median *= 250.0 / exposure

    wavelength = sxpar(flat_header, 'WAVELENG')
    printf, flat_lun, date, times[line_centers[lc]], wavelength, flat_median, $
            format='(%"%s, %f, %0.2f, %f")'
  endfor

  fits_read, dark_fcb, dark_times, dark_times_header, exten_no=dark_fcb.nextend - 1L
  for e = 1L, dark_fcb.nextend - 2L do begin
    fits_read, dark_fcb, dark_data, dark_header, exten_no=e
    ; use mask from last flat above since nothing better available in darks
    dark_median = median(dark_data[mask_ind])
    printf, dark_lun, date, dark_times[e - 1], dark_median, $
            format='(%"%s, %f, %f")'
  endfor
end


pro comp_flatmedian_analysis, process_dir, flat_output_filename, dark_output_filename, $
                              start_date=start_date
  compile_opt strictarr

  if (n_elements(start_date) eq 0L) then begin
    after = '20140100'
  endif else begin
    after = start_date
  endelse

  openw, flat_lun, flat_output_filename, /get_lun
  openw, dark_lun, dark_output_filename, /get_lun

  ; loop over date dirs in process_dir
  dirs = file_search(filepath('*', root=process_dir), $
                     /test_directory, $
                     count=n_dirs)
  for d = 0L, n_dirs - 1L do begin
    date = file_basename(dirs[d])

    skip_date = 0B
    if (date eq '20141016') then skip_date = 1B
    if (date eq '20150803') then skip_date = 1B
    if (date eq '20150821') then skip_date = 1B
    if (date eq '20160510') then skip_date = 1B
    if (date eq '20160511') then skip_date = 1B
    if (date eq '20160701') then skip_date = 1B
    if (date eq '20160712') then skip_date = 1B
    if (date eq '20160720') then skip_date = 1B
    if (date eq '20160721') then skip_date = 1B
    if (date eq '20160723') then skip_date = 1B
    if (date eq '20160724') then skip_date = 1B

    if (~stregex(date, '^[[:digit:]]{8}$', /boolean) || date lt after || skip_date) then begin
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

    ; find the dark.fts file
    dark_filename = filepath('dark.fts', root=dirs[d])
    if (~file_test(dark_filename)) then begin
      dark_filename = filepath('dark.fts', subdir='level1', root=dirs[d])
    endif
    if (~file_test(dark_filename)) then continue

    fits_open, flat_filename, flat_fcb

    fits_read, flat_fcb, wavelengths, wavelengths_header, $
               exten_no=flat_fcb.nextend - 1L
    fits_read, flat_fcb, times, times_header, $
               exten_no=flat_fcb.nextend - 2L

    fits_open, dark_filename, dark_fcb

    eps = 0.01

    line_centers_1074 = where(abs(wavelengths - 1083.00) lt eps, n_line_centers_1074)
    if (n_line_centers_1074 gt 0) then begin
      comp_flatmedian_analysis_lc, line_centers_1074, flat_fcb, dark_fcb, $
                                   date, times, $
                                   flat_lun, dark_lun
    endif

    line_centers_1083 = where(abs(wavelengths - 1074.62) lt eps, n_line_centers_1083)
    if (n_line_centers_1083 gt 0) then begin
      comp_flatmedian_analysis_lc, line_centers_1083, flat_fcb, dark_fcb, $
                                   date, times, $
                                   flat_lun, dark_lun
    endif

    fits_close, flat_fcb
    fits_close, dark_fcb
    flush, flat_lun
    flush, dark_lun
  endfor

  free_lun, flat_lun
  free_lun, dark_lun
end

;process_dir = '/hao/compdata1/Data/CoMP/process.flats'
process_dir = '/hao/mahidata1/Data/CoMP/process'
comp_flatmedian_analysis, process_dir, 'flat-medians-new.csv', 'dark-medians-new.csv', start_date='20160906'

end
