; docformat = 'rst'

;+
; Plot centering for the day.
;
; :Params:
;   dir : in, required, type=string
;     directory that the centering .csv files are in
;   output_filename : in, required, type=string
;     filename of output plot
;   date : in, required, type=string
;     date for plot
;   wave_type : in, required, type=string
;     wavelength to process, '1074', '1079', etc.
;-
pro comp_plot_centering, dir, output_filename, date, wave_type
  compile_opt strictarr

  centering_hash = hash()
  foreach loc, ['ul', 'lr'] do begin
    foreach stop, ['occ', 'field'] do begin
      foreach im, ['image', 'flat'] do begin
        name = string(im, stop, loc, format='(%"%s.%s.%s")')
        filename = filepath(string(date, name, format='(%"%s.comp.%s.csv")'), root=dir)
        if (~file_test(filename)) then begin
          mg_log, '%s file not found, quiting', name, name='comp', /warn
          goto, done
        endif

        centering_hash[name] = read_csv(filename)
      endforeach
    endforeach
  endforeach
  
  image_names = ['wave_type', 'time', 'x', 'y', 'r', 'ind']

  image_occ_ul = mg_convert_structarr(centering_hash['image.occ.ul'], field_names=image_names)
  image_occ_lr = mg_convert_structarr(centering_hash['image.occ.lr'], field_names=image_names)

  image_field_ul = mg_convert_structarr(centering_hash['image.field.ul'], field_names=image_names)
  image_field_lr = mg_convert_structarr(centering_hash['image.field.lr'], field_names=image_names)

  flat_names = ['wave_type', 'time', 'x', 'y', 'r']

  flat_occ_ul = mg_convert_structarr(centering_hash['flat.occ.ul'], field_names=flat_names)
  flat_occ_lr = mg_convert_structarr(centering_hash['flat.occ.lr'], field_names=flat_names)

  flat_field_ul = mg_convert_structarr(centering_hash['flat.field.ul'], field_names=flat_names)
  flat_field_lr = mg_convert_structarr(centering_hash['flat.field.lr'], field_names=flat_names)

  ; filter by wave_type

  wave_ind = where(image_occ_ul.wave_type eq long(wave_type), count)
  if (count eq 0L) then begin
    mg_log, 'no %s centering information to plot', wave_type, name='comp', /warn
    return
  endif

  image_occ_ul = image_occ_ul[wave_ind]
  image_occ_lr = image_occ_lr[wave_ind]

  flat_occ_ul  = flat_occ_ul[wave_ind]
  flat_occ_lr  = flat_occ_lr[wave_ind]

  image_field_ul = image_field_ul[wave_ind]
  image_field_lr = image_field_lr[wave_ind]

  flat_field_ul  = flat_field_ul[wave_ind]
  flat_field_lr  = flat_field_lr[wave_ind]

  mg_psbegin, filename=output_filename, xsize=8.0, ysize=10.5, /inches, $
              /color, xoffset=0.0, yoffset=0.0

  device, decomposed=1
  !p.font = 1

  !p.multi = [0, 2, 6, 0, 0]

  ; plot calculated and flat together

  image_color = 'a0a0a0'x
  flat_color = '0000ff'x
  charsize = 2.0

  ;occ_radius_range = [227.0, 234.0]

  occ_ul_radius_range = [min(image_occ_ul.r) < min(flat_occ_ul.r), $
                         max(image_occ_ul.r) > max(flat_occ_ul.r)]
  occ_lr_radius_range = [min(image_occ_lr.r) < min(flat_occ_lr.r), $
                         max(image_occ_lr.r) > max(flat_occ_lr.r)]

  field_ul_radius_range = [min(image_field_ul.r) < min(flat_field_ul.r), $
                           max(image_field_ul.r) > max(flat_field_ul.r)]
  field_lr_radius_range = [min(image_field_lr.r) < min(flat_field_lr.r), $
                           max(image_field_lr.r) > max(flat_field_lr.r)]

  ;field_radius_range = [298.0, 305.0]
  ;ul_x_range = [286.0, 316.0]
  ;ul_y_range = [710.0, 740.0]
  ;lr_x_range = [690.0, 720.0]
  ;lr_y_range = [303.0, 323.0]

  occ_ul_x_range = [min(image_occ_ul.x) < min(flat_occ_ul.x), $
                    max(image_occ_ul.x) > max(flat_occ_ul.x)]
  occ_ul_y_range = [min(image_occ_ul.y) < min(flat_occ_ul.y), $
                    max(image_occ_ul.y) > max(flat_occ_ul.y)]
  occ_lr_x_range = [min(image_occ_lr.x) < min(flat_occ_lr.x), $
                    max(image_occ_lr.x) > max(flat_occ_lr.x)]
  occ_lr_y_range = [min(image_occ_lr.y) < min(flat_occ_lr.y), $
                    max(image_occ_lr.y) > max(flat_occ_lr.y)]

  field_ul_x_range = [min(image_field_ul.x) < min(flat_field_ul.x), $
                      max(image_field_ul.x) > max(flat_field_ul.x)]
  field_ul_y_range = [min(image_field_ul.y) < min(flat_field_ul.y), $
                      max(image_field_ul.y) > max(flat_field_ul.y)]
  field_lr_x_range = [min(image_field_lr.x) < min(flat_field_lr.x), $
                      max(image_field_lr.x) > max(flat_field_lr.x)]
  field_lr_y_range = [min(image_field_lr.y) < min(flat_field_lr.y), $
                      max(image_field_lr.y) > max(flat_field_lr.y)]

  ; occ UL x
  plot, image_occ_ul.time, image_occ_ul.x, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Occulter UL x-coord', $
        yrange=occ_ul_x_range
  oplot, image_occ_ul.time, image_occ_ul.x, color=image_color, /noclip
  oplot, flat_occ_ul.time, flat_occ_ul.x, color=flat_color, /noclip

  ; field UL x
  plot, image_field_ul.time, image_field_ul.x, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Field UL x-coord', $
        yrange=field_ul_x_range
  oplot, image_field_ul.time, image_field_ul.x, color=image_color, /noclip
  oplot, flat_field_ul.time, flat_field_ul.x, color=flat_color, /noclip

  ; occ UL y
  plot, image_occ_ul.time, image_occ_ul.y, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Occulter UL y-coord', $
        yrange=occ_ul_y_range
  oplot, image_occ_ul.time, image_occ_ul.y, color=image_color, /noclip
  oplot, flat_occ_ul.time, flat_occ_ul.y, color=flat_color, /noclip

  ; field UL y
  plot, image_field_ul.time, image_field_ul.y, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Field UL y-coord', $
        yrange=field_ul_y_range
  oplot, image_field_ul.time, image_field_ul.y, color=image_color, /noclip
  oplot, flat_field_ul.time, flat_field_ul.y, color=flat_color, /noclip

  ; occ UL r
  plot, image_occ_ul.time, image_occ_ul.r, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Occulter UL radius', $
        yrange=occ_ul_radius_range
  oplot, image_occ_ul.time, image_occ_ul.r, color=image_color, /noclip
  oplot, flat_occ_ul.time, flat_occ_ul.r, color=flat_color, /noclip

  ; field UL r
  plot, image_field_ul.time, image_field_ul.r, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Field UL radius', $
        yrange=field_ul_radius_range
  oplot, image_field_ul.time, image_field_ul.r, color=image_color, /noclip
  oplot, flat_field_ul.time, flat_field_ul.r, color=flat_color, /noclip

  ; occ LR x
  plot, image_occ_lr.time, image_occ_lr.x, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Occulter LR x-coord', $
        yrange=occ_lr_x_range
  oplot, image_occ_lr.time, image_occ_lr.x, color=image_color, /noclip
  oplot, flat_occ_lr.time, flat_occ_lr.x, color=flat_color, /noclip

  ; field LR x
  plot, image_field_lr.time, image_field_lr.x, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Field LR x-coord', $
        yrange=field_lr_x_range
  oplot, image_field_lr.time, image_field_lr.x, color=image_color, /noclip
  oplot, flat_field_lr.time, flat_field_lr.x, color=flat_color, /noclip

  ; occ LR y
  plot, image_occ_lr.time, image_occ_lr.y, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Occulter LR y-coord', $
        yrange=occ_lr_y_range
  oplot, image_occ_lr.time, image_occ_lr.y, color=image_color, /noclip
  oplot, flat_occ_lr.time, flat_occ_lr.y, color=flat_color, /noclip

  ; field LR y
  plot, image_field_lr.time, image_field_lr.y, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Field LR y-coord', $
        yrange=field_lr_y_range
  oplot, image_field_lr.time, image_field_lr.y, color=image_color, /noclip
  oplot, flat_field_lr.time, flat_field_lr.y, color=flat_color, /noclip

  ; occ LR r
  plot, image_occ_lr.time, image_occ_lr.r, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Occulter LR radius', $
        yrange=occ_lr_radius_range
  oplot, image_occ_lr.time, image_occ_lr.r, color=image_color, /noclip
  oplot, flat_occ_lr.time, flat_occ_lr.r, color=flat_color, /noclip

  ; field LR r
  plot, image_field_lr.time, image_field_lr.r, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Field LR radius', $
        yrange=field_lr_radius_range
  oplot, image_field_lr.time, image_field_lr.r, color=image_color, /noclip
  oplot, flat_field_lr.time, flat_field_lr.r, color=flat_color, /noclip

  !p.multi = 0
  xyouts, 0.5, 0.98, $
          string(date, wave_type, format='(%"%s (%s nm)")'), $
          alignment=0.5, charsize=1.5, /normal, font=1

  mg_psend

  done:
  obj_destroy, centering_hash
  mg_log, 'done', name='comp', /info
end


; main-level example program

date = '20180304'
eng_basedir = '/hao/mahidata1/Data/CoMP/engineering.latest'
eng_dir = filepath('', subdir=comp_decompose_date(date), root=eng_basedir)
print, eng_dir
comp_plot_centering, eng_dir, date + '.centering-new.ps', date, '1074'

end
