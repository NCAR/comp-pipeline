; docformat = 'rst'

pro comp_plot_rest_wavelength, filename, column
  compile_opt strictarr

  names = ['date', $
           'doppler_shift_0', 'velocity_0', 'line_width_0', 'peak_intensity_0', $
           'doppler_shift_1', 'velocity_1', 'line_width_1', 'peak_intensity_1', $
           'doppler_shift_2', 'velocity_2', 'line_width_2', 'peak_intensity_2', $
           'doppler_shift_22', 'velocity_22', 'line_width_22', 'peak_intensity_22', $
           'doppler_shift_3', 'velocity_3', 'line_width_3', 'peak_intensity_3']
  n_lines = file_lines(filename)

  jds = strarr(n_lines)
  rest_wavelength = fltarr(n_lines)
  line = ''
  openr, lun, filename, /get_lun
  for i = 0L, n_lines - 1L do begin
    readf, lun, line
    tokens = strsplit(line, /extract)
    date_parts = long(comp_decompose_date(tokens[0]))
    jds[i] = julday(date_parts[1], date_parts[2], date_parts[0], 0, 0, 0)
    rest_wavelength[i] = float(tokens[column])
  endfor
  free_lun, lun

  xrange = [jds[0], jds[-1]]
  xtickv = mg_tick_locator(xrange, max_ticks=12L, /months, minor=xminor)

  !null = label_date(date_format='%Y-%m')
  window, xsize=1000, ysize=350, $
          title=string(names[column], format='column: %s'), /free
  plot, jds, rest_wavelength, $
        psym=4, symsize=0.6, $
        title=names[column], $
        xstyle=1, xrange=xrange, xtitle='Dates', $
        xtickv=xtickv, xticks=n_elements(xtickv) - 1L, xminor=xminor, $
        ystyle=1, yrange=[-8.0, -2.0], $
        xtickformat='label_date', $
        ytitle='Rest wavelength [nm]'
end


; main-level example program

filename = 'comp.restwvl.median.synoptic.txt'

comp_plot_rest_wavelength, filename, 2
comp_plot_rest_wavelength, filename, 6
comp_plot_rest_wavelength, filename, 10
comp_plot_rest_wavelength, filename, 14
comp_plot_rest_wavelength, filename, 18

end
