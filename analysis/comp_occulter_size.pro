; docformat = 'rst'

pro comp_occulter_size, start_date, end_date, process_root, $
                        db_filename=db_filename
  compile_opt strictarr

  if (~file_test(db_filename, /regular)) then begin
    print, 'creating new DB file...'
    dates = file_search(filepath('*', root=process_root), count=n_dates)
    dates = file_basename(dates)
    dates = dates[sort(dates)]

    openw, lun, db_filename, /get_lun

    for d = 0L, n_dates - 1L do begin
      if (dates[d] lt start_date) then continue
      if (dates[d] ge end_date) then break
      level1_dir = filepath('', subdir=[dates[d], 'level1'], root=process_root)
      l1_files = file_search(filepath('????????.??????.comp.1074.*.*.fts.gz', $
                                      root=level1_dir), $
                             count=n_l1_files)
      if (n_l1_files eq 0L) then continue

      ind = where(strmatch(file_basename(l1_files), '*bkg*.fts.gz') eq 0L, n_l1_files)
      print, dates[d], n_l1_files, format='(%"%s -> %d L1 files")'
      if (n_l1_files eq 0L) then continue
      l1_files = l1_files[ind]

      radii = fltarr(n_l1_files, 2)
      occulter_change = 0B
      for f = 0L, n_l1_files - 1L do begin
        fits_open, l1_files[f], fcb
        fits_read, fcb, date, primary_header, /header_only, exten_no=0L
        fits_close, fcb

        occulter_id = sxpar(primary_header, 'OCC-ID')
        if (f eq 0L) then begin
          first_occulter_id = occulter_id
        endif else begin
          if (first_occulter_id ne occulter_id) then begin
            occulter_change = 1B
          endif
        endelse
        radii[f, 0] = sxpar(primary_header, 'ORADIUS')
        radii[f, 1] = sxpar(primary_header, 'FRADIUS')
      endfor

      ; don't record a day with an occulter change
      if (occulter_change) then begin
        print, first_occulter_id, occulter_id, $
               format='(%"occulter change from %d to %d")'
      endif else begin
        print, dates[d], mean(radii, dimension=1), occulter_id, $
               format='(%"%s, %0.4f, %0.4f, %d")'
        printf, lun, dates[d], mean(radii, dimension=1), occulter_id, $
                format='(%"%s, %0.4f, %0.4f, %d")'
      endelse
    endfor

    free_lun, lun
  endif else print, 'using existing DB file'

  n_lines = file_lines(db_filename)
  dates = strarr(n_lines)
  occulter_radius = fltarr(n_lines)
  field_radius = fltarr(n_lines)
  occulter_id = lonarr(n_lines)
  openr, lun, db_filename, /get_lun
  line = ''
  for i = 0L, n_lines - 1L do begin
    readf, lun, line
    tokens = strsplit(line, ',', /extract, count=n_tokens)
    dates[i] = strtrim(tokens[0], 2)
    occulter_radius[i] = float(tokens[1])
    field_radius[i] = float(tokens[2])
    occulter_id[i] = long(tokens[3])
  endfor
  free_lun, lun

  unique_indices = uniq(occulter_id, sort(occulter_id))
  for i = 0L, n_elements(unique_indices) - 1L do begin
    ind = where(occulter_id eq occulter_id[unique_indices[i]])
    print, occulter_id[unique_indices[i]], $
           mean(occulter_radius[ind]), $
           median(occulter_radius[ind]), $
           format='(%"id=%d: mean %0.3f, median %0.3f")'
  endfor
  print, mean(field_radius), median(field_radius), $
         format='(%"field: mean %0.3f, median %0.3f")'
 
  do_plot = 1B
  if (keyword_set(do_plot)) then begin
    print, 'making plot of occulter values...'
    !p.multi = [0, 1, 2]
    plot, occulter_radius, xstyle=1, ystyle=1, yrange=[220.0, 240.0]
    plot, field_radius, xstyle=1, ystyle=1, yrange=[290.0, 310.0]
    !p.multi = 0
  endif
end


; main-level example program

start_date = '20170101'
end_date = '20180101'
year = strmid(start_date, 0, 4)
process_root = '/hao/mahidata1/Data/CoMP/process'
comp_occulter_size, start_date, end_date, process_root, $
                    db_filename=string(year, format='(%"occulter-size-%d.csv")')

; for 2017
; id=27: mean 226.162, median 226.217
; id=31: mean 229.071, median 229.106
; id=35: mean 232.339, median 232.300

end
