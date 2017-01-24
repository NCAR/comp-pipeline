; docformat = 'rst'

pro comp_plot_ovrlpang, filename
  compile_opt strictarr

  data = read_csv(filename)

  dates = data.field1
  overlap_angles = data.field2

;  shutoff_date = julday(10, 9, 2015)  ; 20151008 was last day of data
;  ind = where(dates gt shutoff_date)

;  new_dates = dblarr(n_elements(dates) + 1L)

;  new_dates[0:ind[0] - 1] = dates[0:ind[0] - 1]
;  new_dates[ind[0]] = shutoff_date
;  new_dates[ind[0] + 1:*] = dates[ind[0]:*]

;  new_overlap_angles = fltarr(n_elements(overlap_angles) + 1L)
;  new_overlap_angles[0:ind[0] - 1] = overlap_angles[0:ind[0] - 1]
;  new_overlap_angles[ind[0]] = !values.d_nan
;  new_overlap_angles[ind[0] + 1:*] = overlap_angles[ind[0]:*]

;  dates = new_dates
;  overlap_angles = new_overlap_angles

  !null = label_date(date_format='%Y %M %D')

  mg_psbegin, filename='ovrlpang.ps', xsize=11.0, ysize=8.0, /inches, $
              /color, /landscape, xoffset=0.0, yoffset=11.2

  neutral = 45L
  min_angle = min(overlap_angles, max=max_angle)
  min_angle = floor(min_angle)
  max_angle = ceil(max_angle)
  diff = (max_angle - neutral) > (neutral - min_angle)
  max_angle = neutral + diff
  min_angle = neutral - diff

  device, get_decomposed=odec
  device, decomposed=1

  plot, dates, overlap_angles, $
        font=1, title='OVRLPANG - ' + file_basename(filename), $
        ystyle=1, yrange=[min_angle, max_angle], ytitle='OVRLPANG (degrees)', $
        ;yticks=2 * diff, $
        xstyle=1, xtickformat='label_date', xtitle='Date', xticks=7
  oplot, dates, fltarr(n_elements(data)) + neutral, color='a0a0a0'x

  device, decomposed=odec

  mg_psend
end


; main-level example program

filename = 'ovrlpang-process-1074-centering14.csv'
comp_plot_ovrlpang, filename

end
