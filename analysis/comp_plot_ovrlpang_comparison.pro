; docformat = 'rst'

pro comp_plot_overlpang_comparison_wave_type, filename, _extra=e
  compile_opt strictarr

  data = read_csv(filename)

  dates = double(data.field1)
  overlap_angles = float(data.field3)

  cutoffs = [julday(6, 9, 2014) + 0.5, $
             julday(6, 10, 2014) + 0.5, $
             julday(6, 11, 2014) + 0.5, $
             julday(6, 12, 2014) + 0.5]

  new_overlap_angles = mg_insert_nan(dates, overlap_angles, cutoffs, $
                                     new_x=new_dates)
  oplot, new_dates, new_overlap_angles, _extra=e
end


pro comp_plot_ovrlpang_comparison
  compile_opt strictarr

  ;angle_range = [41.0, 49.0]
  angle_range = [46.0, 49.0]
  neutral_angle = 45.0

  date_range = [julday(6, 9, 2014) - 0.5, julday(6, 13, 2014) + 0.5]

  cutoffs = [julday(6, 9, 2014) + 0.5, $
             julday(6, 10, 2014) + 0.5, $
             julday(6, 11, 2014) + 0.5, $
             julday(6, 12, 2014) + 0.5]

  data = read_csv('ovrlpang-process.csv')

  _dates = data.field1
  _overlap_angles = data.field2

  overlap_angles = mg_insert_nan(_dates, _overlap_angles, cutoffs, $
                                 new_x=dates)

  !null = label_date(date_format='%Y %M %D')

  mg_psbegin, filename='ovrlpang.ps', xsize=11.0, ysize=8.0, /inches, $
              /color, /landscape, xoffset=0.0, yoffset=11.2

  device, get_decomposed=odec
  device, decomposed=1

  wave_types = ['1074', '1079']

  plot, dates, overlap_angles, /nodata, $
        font=1, title='OVRLPANG', $
        ystyle=1, yrange=angle_range, ytitle='OVRLPANG (degrees)', $
        yticks=2 * (angle_range[1] - angle_range[0]), $
        xstyle=1, xrange=date_range, $
        xtickformat='label_date', xtitle='Date', xticks=5, $
        ticklen=0.01

  oplot, dates, fltarr(n_elements(data)) + neutral_angle, $
         color='a0a0a0'x, thick=2.0

  wave_colors = ['0000ff'x, 'ff0000'x]
  for w = 0L, n_elements(wave_types) - 1L do begin
    filename = string(wave_types[w], format='(%"ovrlpang-process-%s.csv")')
    comp_plot_overlpang_comparison_wave_type, filename, $
                                              color=wave_colors[w], $
                                              linestyle=0

    filename = string(wave_types[w], format='(%"ovrlpang-reprocess-%s.csv")')
    comp_plot_overlpang_comparison_wave_type, filename, $
                                              color=wave_colors[w], $
                                              linestyle=1
  endfor

  device, decomposed=odec

  mg_psend
end
