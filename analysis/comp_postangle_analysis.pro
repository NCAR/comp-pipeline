; docformat = 'rst'

pro comp_postangle_analysis, process_dir, output_filename, $
                             start_date=start_date, end_date=end_date
  compile_opt strictarr

  openw, lun, output_filename, /get_lun

  center_wavelengths = [1074.62, 1079.78, 1083.0]
  n_wavelengths = n_elements(center_wavelengths)

  beams = [-1, 1]
  n_beams = n_elements(beams)

  dirs = file_search(filepath('*', root=process_dir), $
                     /test_directory, $
                    count=n_dirs)

  for d = 0L, n_dirs - 1L do begin
    date = file_basename(dirs[d])

    catch, error
    if (error ne 0L) then begin
      fits_close, fcb
      mg_log, 'problem with %s, skipping', date, /error
      continue
    endif

    if (ulong64(date) lt ulong64(start_date)) then continue
    if (n_elements(end_date) gt 0L && ulong64(date) ge ulong64(end_date)) then break

    mg_log, 'processing %s', date, /info

    flat_filename = filepath('flat.fts', $
                             subdir='level1', $
                             root=dirs[d])

    fits_open, flat_filename, fcb

    if (fcb.nextend eq 0L) then begin
      fits_close, fcb
      continue
    endif

    fits_read, fcb, time, time_header, exten_no=fcb.nextend - 2L
    fits_read, fcb, wavelength, wavelength_header, exten_no=fcb.nextend - 1L

    for w = 0L, n_wavelengths - 1L do begin
      for b = 0L, n_beams - 1L do begin
        ind = where(wavelength eq beams[b] * center_wavelengths[w], count)
        for i = 0L, count - 1L do begin
          fits_read, fcb, null, header, /header_only, exten_no=ind[i] + 1L
          post_angle1 = sxpar(header, 'POSTANG1')
          post_angle2 = sxpar(header, 'POSTANG2')
          printf, lun, date, time[ind[i]], $
                  beams[b], abs(wavelength[ind[i]]), $
                  post_angle1, post_angle2, $
                  format='(%"%s, %f, %d, %0.2f, %f, %f")'
        endfor
      endfor
    endfor

    fits_close, fcb

    flush, lun
  endfor

  free_lun, lun
end


; main-level example program

process_dir = '/hao/mahidata1/Data/CoMP/process.flats'
comp_postangle_analysis, process_dir, 'post_angle.csv', $
                         start_date='20170101'

end
