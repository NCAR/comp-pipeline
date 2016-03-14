; docformat = 'rst'

pro comp_write_inventory, cal_directory
  compile_opt strictarr

  files = file_search(filepath('*.FTS', root=cal_directory))

  waves = [1075, 1080, 1083]

  keys = list()
  lst  = list()

  for w = 0L, n_elements(waves) - 1L do begin
    cal_info = comp_get_cal_info(files, whichwave=waves[w])
    for f = 0L, n_elements(cal_info.files) - 1L do begin
      lst->add, string(file_basename(cal_info.files[f]), $
                       cal_info.waves[f], $
                       cal_info.cpols[f] ? 'POL' : '', $
                       cal_info.crets[f] ? 'RET' : '', $
                       cal_info.cangs[f], $
                       strjoin(reform(cal_info.mpols[f, *]), ' '), $
                       cal_info.cvers[f] ? '' : 'DARK', $
                       format='(%"%-19s %4d %-3s %-3s %6.1f %-23s %-4s")')
      keys->add, strmid(file_basename(cal_info.files[f]), 9, 6)
    endfor
  endfor

  keys_array = keys->toArray()
  lines = lst->toArray()

  openw, lun, 'cal-inventory.txt', /get_lun
  ind = sort(keys_array)

  foreach i, ind do begin
    printf, lun, lines[i]
  endforeach

  free_lun, lun
end


; main-level example

cal_directory = '/export/data1/Data/CoMP/raw.calibration/20150729/'
comp_write_inventory, cal_directory

end
