; docformat = 'rst'

function comp_plot_centering_image_readfile, file, time
  compile_opt strictarr

  flat = strmid(file_basename(file), 0, 4) eq 'flat'
  n_lines = file_lines(file)
  data = fltarr(flat ? 4 : 5, n_lines)
  openr, lun, file, /get_lun
  readf, lun, data
  free_lun, lun

  hours = strmid(time, 0, 2)
  mins = strmid(time, 2, 2)
  secs = strmid(time, 4, 2)

  decimal_time = float(hours) + float(mins) / 60.0 + float(secs) / 60.0 / 60.0

  !null = min(abs(data[0, *] - decimal_time), min_index)
  return, data[[1:3], min_index]
end


function comp_plot_centering_image_getgeo, eng_dir, date, time
  compile_opt strictarr

  files = ['calc_field_lr.csv', $
           'calc_occ_lr.csv', $
           'flat_field_lr.csv', $
           'flat_occ_lr.csv', $
           'calc_field_ul.csv', $
           'calc_occ_ul.csv', $
           'flat_field_ul.csv', $
           'flat_occ_ul.csv']
  files = ['calc_occ_lr.csv', $
           'flat_occ_lr.csv', $
           'calc_occ_ul.csv', $
           'flat_occ_ul.csv']
  files = filepath(files, subdir=comp_decompose_date(date), root=eng_dir)

  return, {calc_occ_lr: comp_plot_centering_image_readfile(files[0], time), $
           flat_occ_lr: comp_plot_centering_image_readfile(files[1], time), $
           calc_occ_ul: comp_plot_centering_image_readfile(files[2], time), $
           flat_occ_ul: comp_plot_centering_image_readfile(files[3], time)}
end


pro comp_plot_centering_image, date, time, sav_dir, l1_process_dir, dark, geo
  compile_opt strictarr

  restore, filepath(string(date, time, $
                           format='(%"%s.%s.FTS.centering-ul.sav")'), $
                    root=sav_dir)
  restore, filepath(string(date, time, $
                           format='(%"%s.%s.FTS.centering-lr.sav")'), $
                    root=sav_dir)

  fits_open, filepath(string(date, time, format='(%"%s.%s.FTS")'), $
                      root=l1_process_dir), $
             fcb
  fits_read, fcb, data, header, exten_no=1
  fits_close, fcb

  window, xsize=2 * 1024, ysize=1024, /free, $
          title=string(date, time, format='(%"%s.%s.FTS")')

  device, decomposed=1
  occ_color = '00ffff'x
  field_color = 'ffffff'x

  theta = findgen(360) * !dtor

  tv, bytscl(float(data) - dark), 0, 0
  tv, bytscl(float(data) - dark), 1024, 0

  ; UL
  plots, occulter_points1[0, *], occulter_points1[1, *] + 1024 - 620, $
         /device, color=occ_color
;  plots, field_points1[0, *], field_points1[1, *] + 1024 - 620, $
;         /device, color=field_color

  plots, geo.calc_occ_ul[2] * cos(theta) + geo.calc_occ_ul[0] + 1024, $
         geo.calc_occ_ul[2] * sin(theta) + geo.calc_occ_ul[1], $
         /device, color=occ_color
  plots, [geo.calc_occ_ul[0]] + 1024, [geo.calc_occ_ul[1]], psym=1, symsize=1.0, $
         /device, color=occ_color

;  plots, geo.calc_field_ul[2] * cos(theta) + geo.calc_field_ul[0] + 1024, $
;         geo.calc_field_ul[2] * sin(theta) + geo.calc_field_ul[1], $
;         /device, color=field_color
;  plots, [geo.calc_field_ul[0]] + 1024, [geo.calc_field_ul[1]], psym=1, symsize=1.0, $
;         /device, color=field_color

  ; LR
  plots, occulter_points2[0, *] + 1024 - 620, occulter_points2[1, *], $
         /device, color=occ_color
;  plots, field_points2[0, *] + 1024 - 620, field_points2[1, *], $
;         /device, color=field_color

  plots, geo.calc_occ_lr[2] * cos(theta) + geo.calc_occ_lr[0] + 1024, $
         geo.calc_occ_lr[2] * sin(theta) + geo.calc_occ_lr[1], $
         /device, color=occ_color
  plots, [geo.calc_occ_lr[0]] + 1024, [geo.calc_occ_lr[1]], psym=1, symsize=1.0, $
         /device, color=occ_color

;  plots, geo.calc_field_lr[2] * cos(theta) + geo.calc_field_lr[0] + 1024, $
;         geo.calc_field_lr[2] * sin(theta) + geo.calc_field_lr[1], $
;         /device, color=field_color
;  plots, [geo.calc_field_lr[0]] + 1024, [geo.calc_field_lr[1]], psym=1, symsize=1.0, $
;         /device, color=field_color
end


; main-level program

;20150418.092841.FTS
;20150418.092811.FTS

date = '20150418'
;time = '075921'   ; local time
;time = '114004'   ; local time
time = '092811'

flags = '.expan_factor_azi3'

data_dir = '/hao/mahidata1/Data/CoMP'
eng_dir = filepath('engineering' + flags, root=data_dir)
log_dir = filepath('', subdir=comp_decompose_date(date), root=eng_dir)

raw_dir = filepath(date, subdir='raw.expan_factor_azi', root=data_dir)

process_dir = filepath(date, subdir='process' + flags, root=data_dir)
l1_dir = filepath('level1', root=process_dir)

dark_filename = filepath('dark.fts', root=l1_dir)
fits_open, dark_filename, fcb
fits_read, fcb, dark, dark_header, exten_no=1
fits_close, fcb

geo = comp_plot_centering_image_getgeo(eng_dir, date, time)
comp_plot_centering_image, date, time, eng_dir, raw_dir, dark, geo

end
