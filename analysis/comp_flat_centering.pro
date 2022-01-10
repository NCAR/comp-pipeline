; docformat = 'rst'


pro comp_flat_centering_gif, date, filename, data, header
  compile_opt strictarr

  dims = size(data, /dimensions)

  display_min = 0.0
  display_max = 84.0 * 20.0 / comp_transmission(date)
  display_power = 1.0

  n_colors = 254

  set_plot, 'Z'
  device, set_resolution=[dims[0], dims[1]]
  device, decomposed=0

  loadct, 0, /silent, ncolors=n_colors

  image = bytscl((data > 0.0)^display_power, $
                 min=display_min, $
                 max=display_max, $
                 top=n_colors - 1L)

  tv, image

  fieldstop_color = n_colors
  tvlct, 0, 255, 0, fieldstop_color
  occulter_color = n_colors + 1
  tvlct, 255, 255, 0, occulter_color

  tvlct, r, g, b, /get

  t = findgen(361) * !dtor

  oxcnter1 = (sxpar(header, 'OXCNTRU1') - 1.0)
  oycnter1 = (sxpar(header, 'OYCNTRU1') - 1.0)
  oradius1 = sxpar(header, 'ORADU1')
  oxcnter2 = (sxpar(header, 'OXCNTRU2') - 1.0)
  oycnter2 = (sxpar(header, 'OYCNTRU2') - 1.0)
  oradius2 = sxpar(header, 'ORADU2')

  fxcnter1 = (sxpar(header, 'FXCNTRU1') - 1.0)
  fycnter1 = (sxpar(header, 'FYCNTRU1') - 1.0)
  fradius1 = sxpar(header, 'FRADU1')
  fxcnter2 = (sxpar(header, 'FXCNTRU2') - 1.0)
  fycnter2 = (sxpar(header, 'FYCNTRU2') - 1.0)
  fradius2 = sxpar(header, 'FRADU2')

  ox1 = oradius1 * cos(t) + oxcnter1
  oy1 = oradius1 * sin(t) + oycnter1
  ox2 = oradius2 * cos(t) + oxcnter2
  oy2 = oradius2 * sin(t) + oycnter2

  plots, [oxcnter1], [oycnter1], $
         psym=1, /device, color=occulter_color
  plots, ox1, oy1, /device, color=occulter_color
  plots, [oxcnter2], [oycnter2], $
         psym=1, /device, color=occulter_color
  plots, ox2, oy2, /device, color=occulter_color

  fx1 = fradius1 * cos(t) + fxcnter1
  fy1 = fradius1 * sin(t) + fycnter1
  fx2 = fradius2 * cos(t) + fxcnter2
  fy2 = fradius2 * sin(t) + fycnter2

  plots, [fxcnter1], [fycnter1], $
         psym=1, /device, color=fieldstop_color
  plots, fx1, fy1, /device, color=fieldstop_color
  plots, [fxcnter2], [fycnter2], $
         psym=1, /device, color=fieldstop_color
  plots, fx2, fy2, /device, color=fieldstop_color

  print, filename, format='(%"writing %s...")'
  write_gif, filename, tvrd(), r, g, b
end


pro comp_flat_centering_file, date, filename, center_wavelength
  compile_opt strictarr
  @comp_config_common

  if (~file_test(filename, /regular)) then return

  threshold = 0.001
  wave_region = strtrim(long(center_wavelength), 2)

  fits_open, filename, fcb
  for e = 1, fcb.nextend - 3 do begin
    fits_read, fcb, data, header, exten_no=e
    wavelength = sxpar(header, 'WAVELENG')
    if (abs(wavelength - center_wavelength) lt threshold) then begin
      gif_filename = filepath(string(date, wave_region, e, $
                                     format='(%"%s.comp.%s.flat.ext%03d.gif")'), $
                              subdir=comp_decompose_date(date), $
                              root=engineering_dir)
      comp_flat_centering_gif, date, gif_filename, data, header
    endif
  endfor
  fits_close, fcb
end


;+
; Annotate images of 1083.0 nm flats with centering info.
;-
pro comp_flat_centering, start_date, end_date, wavelength, config_filename=config_filename
  compile_opt strictarr
  @comp_config_common

  comp_configuration, config_filename=config_filename

  date = start_date
  while (date ne end_date) do begin
    print, date, format='(%"#### Creating annotated flat images for %s...")'
    comp_update_configuration, date
    comp_initialize, date

    ; find flat file
    l1_dir = filepath('', subdir=[date, 'level1'], root=process_basedir)
    flat_filename = filepath(string(date, format='(%"%s.comp.flat.fts")'), root=l1_dir)

    ; create annotated image
    comp_flat_centering_file, date, flat_filename, wavelength

    date = comp_increment_date(date)
  endwhile
end


config_basename = 'comp.reprocess.cfg'
config_filename = filepath(config_basename, subdir=['..', 'config'], root=mg_src_root())

; start_date = '20121201'
; ;start_date = '20180101'
; end_date = '20180410'
; start_date = '20140101'
; end_date = '20140120'
; wavelength = 1083.0

start_date = '20170101'
end_date = '20180101'
wavelength = 1074.62

comp_flat_centering, start_date, end_date, wavelength, config_filename=config_filename

end
