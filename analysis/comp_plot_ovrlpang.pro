; docformat = 'rst'

pro comp_plot_ovrlpang
  compile_opt strictarr

  data = read_csv('ovrlpang.csv')

  !null = label_date(date_format='%Y %M %D')

  mg_psbegin, filename='ovrlpang.ps', xsize=11.0, ysize=8.0, /inches, $
              /color, /landscape, xoffset=0.0, yoffset=11.2

  neutral = 45L
  min_angle = min(data.field2, max=max_angle)
  min_angle = floor(min_angle)
  max_angle = ceil(max_angle)
  diff = (max_angle - neutral) > (neutral - min_angle)
  max_angle = neutral + diff
  min_angle = neutral - diff

  device, get_decomposed=odec
  device, decomposed=1

  plot, data.field1, data.field2, $
        font=1, title='OVRLPANG', $
        ystyle=1, yrange=[min_angle, max_angle], ytitle='OVRLPANG (degrees)', $
        yticks=2 * diff, $
        xstyle=1, xtickformat='label_date', xtitle='Date', xticks=7
  oplot, data.field1, fltarr(n_elements(data.field1)) + neutral, color='a0a0a0'x

  device, decomposed=odec

  mg_psend
end
