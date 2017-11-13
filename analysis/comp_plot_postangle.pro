; docformat = 'rst'

function comp_plot_postangle_refangle, angle
  compile_opt strictarr

  ; put angle between -360.0 and +360.0
  if (abs(angle) gt 360.0) then angle mod= 360.0

  ; put angle between -180.0 and +180.0
  if (angle gt 180.0) then angle -= 360.0
  if (angle lt -180.0) then angle += 360.0

  return, angle
end


pro comp_plot_postangle, filename
  compile_opt strictarr

  n_lines = file_lines(filename)

  s = replicate({datetime: 0.0D, $
                 wavelength: 0.0, $
                 beam: 0L, $
                 post_angle1: 0.0, $
                 post_angle2: 0.0}, n_lines)

  openr, lun, filename, /get_lun
  line = ''
  for i = 0L, n_lines - 1L do begin
    readf, lun, line
    tokens = strsplit(line, ',', /extract)

    year  = long(strmid(tokens[0], 0, 4))
    month = long(strmid(tokens[0], 4, 2))
    day   = long(strmid(tokens[0], 6, 2))

    s[i].datetime    = julday(month, day, year, float(tokens[1]))
    s[i].beam        = long(tokens[2])
    s[i].wavelength  = float(tokens[3])
    s[i].post_angle1 = comp_plot_postangle_refangle(float(tokens[4]))
    s[i].post_angle2 = comp_plot_postangle_refangle(float(tokens[5]))
  endfor
  free_lun, lun

  center_wavelengths = [1074.62, 1079.78, 1083.0]
  n_wavelengths = n_elements(center_wavelengths)

  beams = [-1, 1]
  n_beams = n_elements(beams)

  !null = label_date(date_format=['%M %D', '%Y'])

  mg_psbegin, filename='post-angle.ps', xsize=8.0, ysize=10.0, /inches, $
              /color, xoffset=0.0, yoffset=0.5

  !p.multi = [0, 2, 6]

  for w = 0L, n_wavelengths - 1L do begin
    for b = 0L, n_beams - 1L do begin
      ind = where(s.wavelength eq center_wavelengths[w] and s.beam eq beams[b], $
                  count)

      title = string(center_wavelengths[w], beams[b], format='(%"%0.2f (beam %d)")')

      ;yrange = [-25.0, 50]
      yrange = [-180.0, 180.0]
      plot, s[ind].datetime, s[ind].post_angle1, $
            title=string(1, title, format='(%"Post angle %d %s")'), $
            xtickformat=['LABEL_DATE', 'LABEL_DATE'], $
            xtickunits=['Time', 'Time'], $
            xminor=12, xticks=12, yminor=1, $
            xticklen=-0.01, yticklen=-0.01, $
            xstyle=9, ystyle=9, $
            yrange=yrange, psym=3
      plot, s[ind].datetime, s[ind].post_angle2, $
            title=string(2, title, format='(%"Post angle %d %s")'), $
            xtickformat=['LABEL_DATE', 'LABEL_DATE'], $
            xtickunits=['Time', 'Time'], $
            xticklen=-0.01, yticklen=-0.01, $
            xminor=12, xticks=12, yminor=1, $
            xstyle=9, ystyle=9, $
            yrange=yrange, psym=3
    endfor
  endfor

  !p.multi = 0
  mg_psend
end


; main-level example program

comp_plot_postangle, 'post_angle.csv'

end
