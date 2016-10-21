; docformat = 'rst'

pro comp_plot_ovrlpang
  compile_opt strictarr

  data = read_csv('ovrlpang.csv')

  !null = label_date(date_format='%Y.%M.%D')

  mg_psbegin, filename='ovrlpang.ps', xsize=11.0, ysize=8.0, /inches, $
              /color, /landscape, xoffset=0.0, yoffset=11.2

  plot, data.field1, data.field2, $
        font=1, title='OVRLPANG in 2016', $
        ystyle=1, yrange=[42.0, 46.0], ytitle='OVRLPANG (degrees)', $
        xstyle=1, xtickformat='label_date', xtitle='Date', xticks=7

  mg_psend
end
