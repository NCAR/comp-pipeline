; docformat = 'rst'

pro comp_plot_backgrounds, filename, wave_region
  compile_opt strictarr

  d = read_csv(filename, types=["Double", "String", "Float"], n_table_header=1)

  jds = d.field1
  backg3wl = d.field3

  window, xsize=1200, ysize=500
  !null = label_date(date_format='%Y-%N')
  plot, jds, backg3wl, $
        title=string(wave_region, format='Backgrounds (BACKG3WL) for %s nm science files'), $
        psym=6, symsize=0.5, $
        xstyle=1, xtickformat='label_date', xtitle='Date', $
        ystyle=1, yrange=[0.0, 200.0], ytitle='Background'
end


; main-level example program

comp_plot_backgrounds, 'background-1079.csv', '1079'

end
