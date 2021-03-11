; docformat = 'rst'

pro comp_analyze_gbu, wave_region, root_dir
  compile_opt strictarr

  gbu_files = file_search(filepath(string(wave_region, format='(%"*.comp.%s.gbu.log")'), $
                                   subdir=['????????', 'level1'], $
                                   root=root_dir), $
                          count=n_gbu_files)
  n_total_files = 0L
  n_lt_threshold = 0L
  n_only_lt_threshold = 0L
  check_threshold = 35.0
  check_list = list()
  check_value = list()
  for f = 0L, n_gbu_files - 1L do begin
    print, f + 1, n_gbu_files, file_basename(gbu_files[f]), format='(%"%d/%d: %s")'
    gbu = comp_read_gbu(gbu_files[f], count=n_files)
    check_indices = where((gbu.reason eq 0) and (gbu.lt_threshold gt check_threshold), n_check)
    print, n_check, format='(%"found %d border-line files to check")'
    if (n_check gt 0L) then begin
      check_list->add, (gbu.l1file)[check_indices], /extract
      check_value->add, (gbu.lt_threshold)[check_indices], /extract
    endif
    n_total_files += n_files
    if (n_files gt 0L) then begin
      indices = where((gbu.reason and 256) gt 0L, n_lt)
      n_lt_threshold += n_lt
      if (n_lt gt 0L) then print, transpose([(gbu.l1file)[indices]])
      only_indices = where(gbu.reason eq 256, n_only_lt)
      n_only_lt_threshold += n_only_lt
      if (n_only_lt gt 0L) then begin
        print, 'only'
        print, (gbu.l1file)[only_indices]
      endif
    endif
  endfor
  print, n_lt_threshold, n_total_files, (100.0 * n_lt_threshold) / n_total_files, $
         format='(%"total: %d/%d files = %0.3f%%")'
  print, n_only_lt_threshold, format='(%"%d files only bad for < threshold")'
  print, check_list->count(), check_threshold, format='(%"%d files over %0.1f%% < 0.1 and not bad")'
  if (check_list->count() gt 0L) then begin
    values = check_value->toArray()
    ind = sort(-values)
    for c = 0L, check_list->count() - 1L do begin
      print, c + 1, check_list[ind[c]], check_value[ind[c]], format='(%"[%d] %s: %0.2f%%")'
    endfor
  endif
  obj_destroy, [check_list, check_value]
end


; main-level example program

comp_analyze_gbu, '1074', '/hao/dawn/Data/CoMP/process.reprocess-check'

end
