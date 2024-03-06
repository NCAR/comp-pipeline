; docformat = 'rst'

;+
; Produce a plot of all the images in a CoMP level 1 science file.
;
; :Params:
;   date : in, required, type=string
;     date in the form "YYYYMMDD"
;   wave_type : in, required, type=string
;     wave region, e.g., "1074"
;   l1_filename : in, required, type=string
;     filename of corresponding level 1 file
;-
pro comp_write_all_iquv_image, date, wave_type, l1_filename
  compile_opt strictarr
  @comp_config_common
  @comp_constants_common

  l1_basename = file_basename(l1_filename)
  comp_read_data, l1_filename, data, headers, primary_header
  dims = size(data, /dimensions)

  n_extensions = dims[2]

  all_wavelengths = fltarr(n_extensions)
  all_polstates = strarr(n_extensions)
  for e = 0L, n_extensions - 1L do begin
    all_wavelengths[e] = sxpar(headers[*, e], 'WAVELENG')
    all_polstates[e] = strtrim(sxpar(headers[*, e], 'POLSTATE'), 2)
  endfor

  pol_states = all_polstates[uniq(all_polstates)]
  ;pol_states = ['I', 'Q', 'U', 'V']

  intensity_indices = where(all_polstates eq 'I', /null)
  wavelengths = all_wavelengths[intensity_indices]
  n_wavelengths = n_elements(wavelengths)

  case 1 of
    n_wavelengths gt 11: reduce_dims_factor = 4L
    n_wavelengths gt 5: reduce_dims_factor = 4L
    else: reduce_dims_factor = 2L
  endcase

  mg_log, 'n_wavelengths: %d', n_wavelengths, name='comp', /debug
  mg_log, 'reduce_dims_factor: %d', reduce_dims_factor, name='comp', /debug

  output_dirname = filepath('', $
                        subdir=[date, 'level1'], $
                        root=process_basedir)

  iquv_basename_format = file_basename(l1_basename, '.fts.gz')
  iquv_basename_format += '.all.png'
  iquv_filename_format = filepath(iquv_basename_format, root=output_dirname)

  case wave_type of
    '1074': begin
        intensity_display_min   = dispmin1074
        intensity_display_max   = dispmax1074
        intensity_display_gamma = 0.7
        intensity_display_power = dispexp1074
      end
    '1079': begin
        intensity_display_min   = dispmin1079
        intensity_display_max   = dispmax1079
        intensity_display_gamma = 0.7
        intensity_display_power = dispexp1079
      end
    '1083': begin
        intensity_display_min   = dispmin1083
        intensity_display_max   = dispmax1083
        intensity_display_gamma = 0.7
        intensity_display_power = dispexp1083
      end
  endcase

  qu_display_min   = -0.1
  qu_display_max   =  0.1
  qu_display_gamma =  1.0
  qu_display_power =  1.0

  v_display_min    = -0.1
  v_display_max    =  0.1
  v_display_gamma  =  1.0
  v_display_power  =  1.0

  datetime = strmid(l1_basename, 0, 15)
  date_tokens = comp_decompose_date(strmid(datetime, 0, 8))
  time_tokens = comp_decompose_time(strmid(datetime, 9, 6))
  date_stamp = string(date_tokens, time_tokens, format='(%"%s-%s-%sT%s:%s:%sZ")')

  original_device = !d.name
  set_plot, 'Z'
  device, get_decomposed=original_decomposed
  tvlct, original_rgb, /get
  device, decomposed=0, $
          set_pixel_depth=24, $
          set_resolution=[n_wavelengths * nx, n_elements(pol_states) * ny] / reduce_dims_factor

  mg_log, 'creating image of size: [%d, %d]', [n_wavelengths * nx, n_elements(pol_states) * ny] / reduce_dims_factor, $
          name='comp', /debug

  n_colors = 252

  text_color = 252
  occulter_color = 253
  guess_color = 254
  inflection_color = 255

  tvlct, 255, 255, 255, text_color
  tvlct, 0, 255, 255, occulter_color
  tvlct, 255, 255, 0, guess_color
  tvlct, 255, 0, 0, inflection_color

  xmargin = 0.05
  ymargin = 0.03

  charsize = 1.0
  title_charsize = 1.25
  detail_charsize = 0.9

  wavelength_tolerance = 0.001

  for w = 0L, n_wavelengths - 1L do begin
    for p = 0L, n_elements(pol_states) - 1L do begin
      panel_indices = where(all_polstates eq pol_states[p] $
                            and abs(all_wavelengths - wavelengths[w]) lt wavelength_tolerance, $
                            n_panel)

      if (n_panel eq 0L) then continue
      panel_data = reform(data[*, *, panel_indices[0]])
      mg_log, 'displaying %s @ %0.2f nm', pol_states[p], wavelengths[w], $
              name='comp', /debug

      if (p eq 0) then begin
        display_min = intensity_display_min
        display_max = intensity_display_max
        display_gamma = intensity_display_gamma
        display_power = intensity_display_power
      endif else if (p eq 3) then begin
        display_min = v_display_min
        display_max = v_display_max
        display_gamma = v_display_gamma
        display_power = v_display_power
      endif else begin
        display_min = qu_display_min
        display_max = qu_display_max
        display_gamma = qu_display_gamma
        display_power = qu_display_power
      endelse

      loadct, 0, ncolors=n_colors, /silent
      mg_gamma_ct, display_gamma, /current, n_colors=n_colors

      im = rebin(panel_data, $
                 nx / reduce_dims_factor, $
                 ny / reduce_dims_factor)

      mask = comp_display_mask([nx, ny] / reduce_dims_factor, $
                               occulter_radius=sxpar(primary_header, 'ORADIUS') / reduce_dims_factor, $
                               field_radius=sxpar(primary_header, 'FRADIUS') / reduce_dims_factor)

      scaled_im = bytscl(mg_signed_power(im * mask, display_power), $
                         min=mg_signed_power(display_min, display_power), $
                         max=mg_signed_power(display_max, display_power), $
                         top=n_colors - 1L, $
                         /nan)

      tv, scaled_im, p * n_wavelengths + w

      if (p eq 0L and w eq 0L) then begin
        title = string(wave_type, format='(%"%s nm")')
        xyouts, xmargin * nx / reduce_dims_factor, $
                (n_elements(pol_states) - 2.5 * ymargin) * ny / reduce_dims_factor, $
                /device, $
                title, $
                charsize=charsize, color=text_color
        xyouts, xmargin * nx / reduce_dims_factor, $
                (n_elements(pol_states) - 1.0 + ymargin) * ny / reduce_dims_factor, $
                /device, $
                date_stamp, $
                charsize=charsize, color=text_color
      endif

      if (p eq 0L) then begin
        xyouts, (w + 0.5) * nx / reduce_dims_factor, $
                (n_elements(pol_states) - 0.35) * ny / reduce_dims_factor, $
                /device, alignment=0.5, $
                string(wavelengths[w], format='(%"%0.2f nm")'), $
                charsize=charsize, color=text_color
      endif
      if (w eq 0L) then begin
        xyouts, (w + 0.5) * nx / reduce_dims_factor, $
                (n_elements(pol_states) - p - 0.5) * ny / reduce_dims_factor, $, $
                /device, alignment=0.5, $
                pol_states[p], $
                charsize=charsize, color=text_color
      endif
    endfor
  endfor

  iquv_filename = iquv_filename_format
  write_png, iquv_filename, tvrd(true=1)
  mg_log, 'wrote %s', file_basename(iquv_filename), name='comp', /debug

  done:
  gamma_ct, 1.0, /current   ; reset gamma to linear ramp
  tvlct, original_rgb
  device, decomposed=original_decomposed
  set_plot, original_device
end


; main-level example program

@comp_config_common

wave_types = ['1074', '1079']

config_basename = 'comp.reprocess-check-2012.cfg'
config_filename = filepath(config_basename, $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())

comp_configuration, config_filename=config_filename

dates = comp_expand_date_expr(date_pattern, count=n_dates)

for d = 0L, n_dates - 1L do begin
  print, dates[d], format='### Processing %s'

  comp_update_configuration, dates[d]
  comp_initialize, dates[d]

  for w = 0L, n_elements(wave_types) - 1L do begin
    l1_basename = string(wave_types[w], $
                         format='*.comp.%s.i*.{3,5,7,9,11,21}.fts.gz')
    l1_filename = filepath(l1_basename, $
                           subdir=[dates[d], 'level1'], $
                           root=process_basedir)
    l1_filenames = file_search(l1_filename, count=n_l1_filenames)

    for f = 0L, n_l1_filenames - 1L do begin
      comp_write_all_iquv_image, dates[d], wave_types[w], l1_filenames[f]
    endfor
  endfor
endfor

end
