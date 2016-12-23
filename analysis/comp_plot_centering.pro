; docformat = 'rst'

pro comp_plot_centering, dir, output_filename, note
  compile_opt strictarr

  calc_occ_ul = read_csv(filepath('calc_occ_ul.csv', root=dir))
  calc_occ_lr = read_csv(filepath('calc_occ_lr.csv', root=dir))
  calc_field_ul = read_csv(filepath('calc_field_ul.csv', root=dir))
  calc_field_lr = read_csv(filepath('calc_field_lr.csv', root=dir))

  flat_occ_ul = read_csv(filepath('flat_occ_ul.csv', root=dir))
  flat_occ_lr = read_csv(filepath('flat_occ_lr.csv', root=dir))
  flat_field_ul = read_csv(filepath('flat_field_ul.csv', root=dir))
  flat_field_lr = read_csv(filepath('flat_field_lr.csv', root=dir))

  mg_psbegin, filename=output_filename, xsize=8.0, ysize=10.5, /inches, $
              /color, xoffset=0.0, yoffset=0.0

  device, decomposed=1
  !p.font = 1

  ; column 1: occulter centers, column 2: field centers
  !p.multi = [0, 2, 6, 0, 1]

  ; plot calculated and flat together

  calc_color = 'a0a0a0'x
  flat_color = '0000ff'x
  charsize = 1.1

  ; occ UL x
  plot, calc_occ_ul.field1, calc_occ_ul.field2, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Occulter UL x-coord', $
        yrange=mg_range([calc_occ_ul.field2, flat_occ_ul.field2])
  oplot, calc_occ_ul.field1, calc_occ_ul.field2, color=calc_color
  oplot, flat_occ_ul.field1, flat_occ_ul.field2, color=flat_color

  ; occ UL y
  plot, calc_occ_ul.field1, calc_occ_ul.field3, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Occulter UL y-coord', $
        yrange=mg_range([calc_occ_ul.field3, flat_occ_ul.field3])
  oplot, calc_occ_ul.field1, calc_occ_ul.field3, color=calc_color
  oplot, flat_occ_ul.field1, flat_occ_ul.field3, color=flat_color

  ; occ UL r
  plot, calc_occ_ul.field1, calc_occ_ul.field4, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Occulter UL radius', $
        yrange=mg_range([calc_occ_ul.field4, flat_occ_ul.field4])
  oplot, calc_occ_ul.field1, calc_occ_ul.field4, color=calc_color
  oplot, flat_occ_ul.field1, flat_occ_ul.field4, color=flat_color

  ; occ LR x
  plot, calc_occ_lr.field1, calc_occ_lr.field2, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Occulter LR x-coord', $
        yrange=mg_range([calc_occ_lr.field2, flat_occ_lr.field2])
  oplot, calc_occ_lr.field1, calc_occ_lr.field2, color=calc_color
  oplot, flat_occ_lr.field1, flat_occ_lr.field2, color=flat_color

  ; occ LR y
  plot, calc_occ_lr.field1, calc_occ_lr.field3, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Occulter LR y-coord', $
        yrange=mg_range([calc_occ_lr.field3, flat_occ_lr.field3])
  oplot, calc_occ_lr.field1, calc_occ_lr.field3, color=calc_color
  oplot, flat_occ_lr.field1, flat_occ_lr.field3, color=flat_color

  ; occ LR r
  plot, calc_occ_lr.field1, calc_occ_lr.field4, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Occulter LR radius', $
        yrange=mg_range([calc_occ_lr.field4, flat_occ_lr.field4])
  oplot, calc_occ_lr.field1, calc_occ_lr.field4, color=calc_color
  oplot, flat_occ_lr.field1, flat_occ_lr.field4, color=flat_color

  ; field UL x
  plot, calc_field_ul.field1, calc_field_ul.field2, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Field UL x-coord', $
        yrange=mg_range([calc_field_ul.field2, flat_field_ul.field2])
  oplot, calc_field_ul.field1, calc_field_ul.field2, color=calc_color
  oplot, flat_field_ul.field1, flat_field_ul.field2, color=flat_color

  ; field UL y
  plot, calc_field_ul.field1, calc_field_ul.field3, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Field UL y-coord', $
        yrange=mg_range([calc_field_ul.field3, flat_field_ul.field3])
  oplot, calc_field_ul.field1, calc_field_ul.field3, color=calc_color
  oplot, flat_field_ul.field1, flat_field_ul.field3, color=flat_color

  ; field UL r
  plot, calc_field_ul.field1, calc_field_ul.field4, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Field UL radius', $
        yrange=mg_range([calc_field_ul.field4, flat_field_ul.field4])
  oplot, calc_field_ul.field1, calc_field_ul.field4, color=calc_color
  oplot, flat_field_ul.field1, flat_field_ul.field4, color=flat_color

  ; field LR x
  plot, calc_field_lr.field1, calc_field_lr.field2, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Field LR x-coord', $
        yrange=mg_range([calc_field_lr.field2, flat_field_lr.field2])
  oplot, calc_field_lr.field1, calc_field_lr.field2, color=calc_color
  oplot, flat_field_lr.field1, flat_field_lr.field2, color=flat_color

  ; field LR y
  plot, calc_field_lr.field1, calc_field_lr.field3, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Field LR y-coord', $
        yrange=mg_range([calc_field_lr.field3, flat_field_lr.field3])
  oplot, calc_field_lr.field1, calc_field_lr.field3, color=calc_color
  oplot, flat_field_lr.field1, flat_field_lr.field3, color=flat_color

  ; field LR r
  plot, calc_field_lr.field1, calc_field_lr.field4, /nodata, $
        xstyle=9, ystyle=9, charsize=charsize, $
        xtitle='time (hours)', ytitle='pixels', $
        title='Field LR radius', $
        yrange=mg_range([calc_field_lr.field4, flat_field_lr.field4])
  oplot, calc_field_lr.field1, calc_field_lr.field4, color=calc_color
  oplot, flat_field_lr.field1, flat_field_lr.field4, color=flat_color

  !p.multi = 0
  xyouts, 0.5, 0.995, output_filename + '!C!8' + note + '!X', $
          alignment=0.5, charsize=1.1, /normal, font=1

  mg_psend
end


; main-level example program

;date = '20161112'
date = '20161114'
n = 8
flags = '.centering' + strtrim(n, 2)
note = 'drad=30.0'

dir = filepath('', subdir=comp_decompose_date(date), $
               root='/hao/mahidata1/Data/CoMP/engineering' + flags)
output_filename = string(n, date, format='(%"centering%d-%s.ps")')

print, 'Writing to ' + output_filename
comp_plot_centering, dir, output_filename, note

end
