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

  calc_occ_ul = read_csv(filepath('calc_occ_ul.csv', root=dir))
  calc_occ_lr = read_csv(filepath('calc_occ_lr.csv', root=dir))
;  calc_field_ul = read_csv(filepath('calc_field_ul.csv', root=dir))
;  calc_field_lr = read_csv(filepath('calc_field_lr.csv', root=dir))

  flat_occ_ul = read_csv(filepath('flat_occ_ul.csv', root=dir))
  flat_occ_lr = read_csv(filepath('flat_occ_lr.csv', root=dir))
;  flat_field_ul = read_csv(filepath('flat_field_ul.csv', root=dir))
;  flat_field_lr = read_csv(filepath('flat_field_lr.csv', root=dir))

  calc_names = ['wave_type', 'time', 'x', 'y', 'r', 'ind']
  calc_occ_ul = mg_convert_structarr(calc_occ_ul, field_names=calc_names)
  calc_occ_lr = mg_convert_structarr(calc_occ_lr, field_names=calc_names)

  flat_names = ['wave_type', 'time', 'x', 'y', 'r']
  flat_occ_ul = mg_convert_structarr(flat_occ_ul, field_names=flat_names)
  flat_occ_lr = mg_convert_structarr(flat_occ_lr, field_names=flat_names)

  ; filter by wave_type

  wave_ind = where(calc_occ_ul.wave_type eq long(wave_type), count)
  if (count eq 0L) then begin
    mg_log, 'no %s centering information to plot', wave_type, name='comp', /warn
  endif

  calc_occ_ul = calc_occ_ul[wave_ind]
  calc_occ_lr = calc_occ_lr[wave_ind]
  flat_occ_ul = flat_occ_ul[wave_ind]
  flat_occ_lr = flat_occ_lr[wave_ind]

  ; sort by time

  ;ind = sort(calc_occ_ul.time)
  ;calc_occ_ul = calc_occ_ul[ind]
  ;calc_occ_lr = calc_occ_lr[ind]
  ;flat_occ_ul = flat_occ_ul[ind]
  ;flat_occ_lr = flat_occ_lr[ind]

  mg_psbegin, filename=output_filename, xsize=8.0, ysize=10.5, /inches, $
              /color, xoffset=0.0, yoffset=0.0

  device, decomposed=1
  !p.font = 1

  !p.multi = [0, 1, 6, 0, 1]

  ; plot calculated and flat together

  calc_color = 'a0a0a0'x
  flat_color = '0000ff'x
  charsize = 1.1

  ;occ_radius_range = [227.0, 234.0]
  occ_ul_radius_range = [min(calc_occ_ul.r) < min(flat_occ_ul.r), $
                         max(calc_occ_ul.r) > max(flat_occ_ul.r)]
  occ_lr_radius_range = [min(calc_occ_lr.r) < min(flat_occ_lr.r), $
                         max(calc_occ_lr.r) > max(flat_occ_lr.r)]
  ;field_radius_range = [298.0, 305.0]
  ;ul_x_range = [286.0, 316.0]
  ul_x_range = [min(calc_occ_ul.x) < min(flat_occ_ul.x), $
                max(calc_occ_ul.x) > max(flat_occ_ul.x)]
  ;ul_y_range = [710.0, 740.0]
  ul_y_range = [min(calc_occ_ul.y) < min(flat_occ_ul.y), $
                max(calc_occ_ul.y) > max(flat_occ_ul.y)]
  ;lr_x_range = [690.0, 720.0]
  lr_x_range = [min(calc_occ_lr.x) < min(flat_occ_lr.x), $
                max(calc_occ_lr.x) > max(flat_occ_lr.x)]
  ;lr_y_range = [303.0, 323.0]
  lr_y_range = [min(calc_occ_lr.y) < min(flat_occ_lr.y), $
                max(calc_occ_lr.y) > max(flat_occ_lr.y)]

  ; occ UL x
  plot, calc_occ_ul.time, calc_occ_ul.x, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Occulter UL x-coord', $
        yrange=ul_x_range
  oplot, calc_occ_ul.time, calc_occ_ul.x, color=calc_color, /noclip
  oplot, flat_occ_ul.time, flat_occ_ul.x, color=flat_color, /noclip

  ; occ UL y
  plot, calc_occ_ul.time, calc_occ_ul.y, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Occulter UL y-coord', $
        yrange=ul_y_range
  oplot, calc_occ_ul.time, calc_occ_ul.y, color=calc_color, /noclip
  oplot, flat_occ_ul.time, flat_occ_ul.y, color=flat_color, /noclip

  ; occ UL r
  plot, calc_occ_ul.time, calc_occ_ul.r, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Occulter UL radius', $
        yrange=occ_ul_radius_range
  oplot, calc_occ_ul.time, calc_occ_ul.r, color=calc_color, /noclip
  oplot, flat_occ_ul.time, flat_occ_ul.r, color=flat_color, /noclip

  ; occ LR x
  plot, calc_occ_lr.time, calc_occ_lr.x, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Occulter LR x-coord', $
        yrange=lr_x_range
  oplot, calc_occ_lr.time, calc_occ_lr.x, color=calc_color, /noclip
  oplot, flat_occ_lr.time, flat_occ_lr.x, color=flat_color, /noclip

  ; occ LR y
  plot, calc_occ_lr.time, calc_occ_lr.y, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Occulter LR y-coord', $
        yrange=lr_y_range
  oplot, calc_occ_lr.time, calc_occ_lr.y, color=calc_color, /noclip
  oplot, flat_occ_lr.time, flat_occ_lr.y, color=flat_color, /noclip

  ; occ LR r
  plot, calc_occ_lr.time, calc_occ_lr.r, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Occulter LR radius', $
        yrange=occ_lr_radius_range
  oplot, calc_occ_lr.time, calc_occ_lr.r, color=calc_color, /noclip
  oplot, flat_occ_lr.time, flat_occ_lr.r, color=flat_color, /noclip

  ; field UL x
;  plot, calc_field_ul.time, calc_field_ul.x, /nodata, $
;        xstyle=9, ystyle=9, charsize=charsize, $
;        xtitle='time (hours)', ytitle='pixels', $
;        title='Field UL x-coord', $
;        yrange=ul_x_range
;  oplot, calc_field_ul.time, calc_field_ul.x, color=calc_color, /noclip
;  oplot, flat_field_ul.time, flat_field_ul.x, color=flat_color, /noclip

  ; field UL y
;  plot, calc_field_ul.time, calc_field_ul.y, /nodata, $
;        xstyle=9, ystyle=9, charsize=charsize, $
;        xtitle='time (hours)', ytitle='pixels', $
;        title='Field UL y-coord', $
;        yrange=ul_y_range
;  oplot, calc_field_ul.time, calc_field_ul.y, color=calc_color, /noclip
;  oplot, flat_field_ul.time, flat_field_ul.y, color=flat_color, /noclip

  ; field UL r
;  plot, calc_field_ul.time, calc_field_ul.r, /nodata, $
;        xstyle=9, ystyle=9, charsize=charsize, $
;        xtitle='time (hours)', ytitle='pixels', $
;        title='Field UL radius', $
;        yrange=field_radius_range
;  oplot, calc_field_ul.time, calc_field_ul.r, color=calc_color, /noclip
;  oplot, flat_field_ul.time, flat_field_ul.r, color=flat_color, /noclip

  ; field LR x
;  plot, calc_field_lr.time, calc_field_lr.x, /nodata, $
;        xstyle=9, ystyle=9, charsize=charsize, $
;        xtitle='time (hours)', ytitle='pixels', $
;        title='Field LR x-coord', $
;        yrange=lr_x_range
;  oplot, calc_field_lr.time, calc_field_lr.x, color=calc_color, /noclip
;  oplot, flat_field_lr.time, flat_field_lr.x, color=flat_color, /noclip

  ; field LR y
;  plot, calc_field_lr.time, calc_field_lr.y, /nodata, $
;        xstyle=9, ystyle=9, charsize=charsize, $
;        xtitle='time (hours)', ytitle='pixels', $
;        title='Field LR y-coord', $
;        yrange=lr_y_range
;  oplot, calc_field_lr.time, calc_field_lr.y, color=calc_color, /noclip
;  oplot, flat_field_lr.time, flat_field_lr.y, color=flat_color, /noclip

  ; field LR r
;  plot, calc_field_lr.time, calc_field_lr.r, /nodata, $
;        xstyle=9, ystyle=9, charsize=charsize, $
;        xtitle='time (hours)', ytitle='pixels', $
;        title='Field LR radius', $
;        yrange=field_radius_range
;  oplot, calc_field_lr.time, calc_field_lr.r, color=calc_color, /noclip
;  oplot, flat_field_lr.time, flat_field_lr.r, color=flat_color, /noclip

  !p.multi = 0
  xyouts, 0.95, 0.985, $
          string(date, wave_type, format='(%"Centering for %s (%s nm)")'), $
          alignment=1.0, charsize=1.1, /normal, font=1

  mg_psend
end


; main-level example program

;dates = '20161111'
;dates = ['20161110', $
;         '20161111', $
;         '20161112', $
;         '20161113', $
;         '20161114', $
;         '20161115', $
;         '20161116', $
;         '20161117', $
;         '20161118']
;dates = ['20161112', $
;         '20161113', $
;         '20161114', $
;         '20161115', $
;         '20161116', $
;         '20161117']
;flag_name = 'centering'
;flag_number = 21


comp_plot_centering, '/hao/mahidata1/Data/CoMP/engineering.latest2/2017/10/01', $
                     '20171001.centering2.ps', $
                     '20171001', '1074'

;dates = ['20150418']
;flag_name = 'expan_factor_azi'
;flag_number = 4

;flags = string(flag_name, flag_number, format='(%".%s%d")')
;note = 'find center from unflat corrected images; modified epoch.cfg'

;for d = 0L, n_elements(dates) - 1L do begin
;  dir = filepath('', subdir=comp_decompose_date(dates[d]), $
;                 root='/hao/mahidata1/Data/CoMP/engineering' + flags)
;  output_filename = string(flags, dates[d], format='(%"centering%s-%s.ps")')
;
;  print, 'Writing to ' + output_filename
;  comp_plot_centering, dir, output_filename, note
;endfor

end
