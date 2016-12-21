; docformat = 'rst'

pro comp_plot_centering_image, date, time, sav_dir, l1_process_dir
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

  window, xsize=1024, ysize=1024, /free, $
          title=string(date, time, format='(%"%s.%s.FTS")')

  device, decomposed=1
  occ_color = '00ffff'x
  field_color = 'ffffff'x

  tv, bytscl(data)

  plots, occulter_points1[0, *], occulter_points1[1, *] + 1024 - 620, $
         /device, color=occ_color
  plots, field_points1[0, *], field_points1[1, *] + 1024 - 620, $
         /device, color=field_color

  plots, occulter_points2[0, *] + 1024 - 620, occulter_points2[1, *], $
         /device, color=occ_color
  plots, field_points2[0, *] + 1024 - 620, field_points2[1, *], $
         /device, color=field_color

end


; main-level program

date = '20161112'
;time = '075921'   ; local time
;time = '114004'   ; local time
time = '074631'

flags = '.centering3'

data_dir = '/hao/mahidata1/Data/CoMP'
eng_dir = filepath('engineering' + flags, root=data_dir)
log_dir = filepath('', subdir=comp_decompose_date(date), root=eng_dir)
raw_dir = filepath(date, subdir='raw', root=data_dir)

comp_plot_centering_image, date, time, eng_dir, raw_dir

end
