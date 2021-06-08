; docformat = 'rst'

pro comp_redo_gifs, config_filename
  compile_opt strictarr
  @comp_config_common

  comp_configuration, config_filename=config_filename, error=error

  dates = file_search(filepath('????????', root=process_basedir), count=n_dates)
  for d = 0L, n_dates - 1L do begin
    date = file_basename(dates[d])
    print, date, format='(%"%s...")'

    comp_initialize, date
    comp_extract_intensity, date, '1074', error=error
    comp_extract_intensity, date, '1074', error=error, /background
  endfor
end


; main-level example

comp_redo_gifs, filepath('comp.reprocess.cfg', $
                         subdir=['..', 'config'], $
                         root=mg_src_root())

end
