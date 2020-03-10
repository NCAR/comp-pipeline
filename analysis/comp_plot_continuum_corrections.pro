; docformat = 'rst'

function comp_plot_continuum_corrections_model, x, a
  compile_opt strictarr

;  return, a[0] * sin(a[1] * x + a[2]) + a[3]
  return, [[a[0] * sin(a[1] * x + a[2]) + a[3]], $
           [sin(a[1] * x + a[2])], $
           [a[0] * x * cos(a[1] * x + a[2])], $
           [a[0] * cos(a[1] * x + a[2])], $
           [x * 0 + 1.0]]
end


pro comp_plot_continuum_corrections, filename
  compile_opt strictarr

  n_lines = file_lines(filename)

  data = strarr(n_lines)
  openr, lun, filename, /get_lun
  readf, lun, data
  free_lun, lun

  dates = strarr(n_lines)
  hours = fltarr(n_lines)
  offsets = fltarr(2, n_lines)
  for i = 0L, n_lines - 1L do begin
    tokens = strsplit(data[i], /extract)
    dates[i] = tokens[0]
    hours[i] = float(tokens[1])
    offsets[*, i] = float(tokens[2:3])
  endfor

  years = long(strmid(dates, 0, 4))
  months = long(strmid(dates, 4, 2))
  days = long(strmid(dates, 6, 2))
  jds = julday(months, days, years, hours)

  !null = label_date(date_format='%Y-%N')

  xsize = 1200
  ysize = 400
  colors = ['0000ff'x, '00ff00'x]
  psyms = [4, 5]
  charsize = 1.1

  beam1_offsets = reform(offsets[0, 0:*:2])
  beam1_jds     = jds[0:*:2]
  beam2_offsets = reform(offsets[0, 1:*:2])
  beam2_jds     = jds[1:*:2]

  good_range = [0.02, 0.05]

  title = string(file_basename(filename), format='(%"Offsets for %s")')
  window, xsize=xsize, ysize=ysize, /free, title=title
  plot, jds, reform(offsets[0, *]), /nodata, $
        color='000000'x, background='ffffff'x, $
        xstyle=1, xtickformat='label_date', xtitle='dates', $
        ystyle=1, yrange=good_range, ytitle='offset', $
        title=title, charsize=charsize
  xyouts, 0.1, 0.9, 'red=beam 1!Cgreen=beam 2', /normal, $
          color='000000'x, charsize=charsize
  oplot, beam1_jds, beam1_offsets, psym=psyms[0], symsize=0.75, color=colors[0]
  oplot, beam2_jds, beam2_offsets, psym=psyms[1], symsize=0.75, color=colors[1]

  beam1_indices = where(beam1_offsets gt good_range[0] and beam1_offsets lt good_range[1], $
                        n_beam1)
  beam2_indices = where(beam2_offsets gt good_range[0] and beam2_offsets lt good_range[1], $
                        n_beam2)

  fit_sine = 0B
  if (keyword_set(fit_sine)) then begin
    fit_scale = 1000.0d
    parameters = [0.01d * fit_scale, $
                  2.0d * !dpi / 365.25d, $
                  0.0d, $
                  0.035d * fit_scale]

    new_parameters1 = mpfitfun('comp_plot_continuum_corrections_model', $
                              beam1_jds[beam1_indices] - jds[0], $
                              fit_scale * beam1_offsets[beam1_indices], $
                              fit_scale * 0.0005, $
                              parameters, $
                              status=status, errmsg=errmsg, niter=n_iterations)
    new_parameters2 = mpfitfun('comp_plot_continuum_corrections_model', $
                              beam2_jds[beam2_indices] - jds[0], $
                              fit_scale * beam2_offsets[beam2_indices], $
                              fit_scale * 0.0005, $
                              parameters, $
                              status=status, errmsg=errmsg, niter=n_iterations)
    results1 = comp_plot_continuum_corrections_model(beam1_jds, new_parameters1)
    results2 = comp_plot_continuum_corrections_model(beam2_jds, new_parameters2)
    plots, beam1_jds, results1[*, 0] / fit_scale, $
           color=colors[0]
    plots, beam2_jds, results2[*, 0] / fit_scale, $
           color=colors[1]
  endif

  coeffs1 = linfit(beam1_jds[beam1_indices] - jds[0], beam1_offsets[beam1_indices])
  offset_fit1 = coeffs1[0] + coeffs1[1] * (beam1_jds - jds[0])

  coeffs2 = linfit(beam2_jds[beam2_indices] - jds[0], beam2_offsets[beam2_indices])
  offset_fit2 = coeffs2[0] + coeffs2[1] * (beam2_jds - jds[0])

  xyouts, 0.75, 0.8, /normal, $
          string(coeffs1, coeffs2, $
                 format='beam 1: %0.4f + %g * t!Cbeam 2: %0.4f + %g * t'), $
          charsize=1.1, color='000000f'x

  plots, beam1_jds, offset_fit1, $
        color=colors[0]
  plots, beam2_jds, offset_fit2, $
        color=colors[1]

  title = string(file_basename(filename), format='(%"H!D2!NO for %s")')
  window, xsize=xsize, ysize=ysize, /free, title=title
  plot, jds, reform(offsets[1, *]), /nodata, $
        color='000000'x, background='ffffff'x, $
        xstyle=1, xtickformat='label_date', xtitle='dates', $
        ystyle=1, yrange=[-0.25, 2.5], ytitle='offset', $
        title=title, charsize=charsize
  xyouts, 0.1, 0.9, 'red=beam 1!Cgreen=beam 2', /normal, $
          color='000000'x, charsize=charsize
  for b = 0L, 1L do begin
    oplot, jds[b:*:2], offsets[1, b:*:2], $
           psym=psyms[b], symsize=0.75, color=colors[b]
  endfor

  offset_limit = 0.01

  title = string(file_basename(filename), $
                 format='(%"Distribution of offset differences by beam for %s")')
  window, xsize=xsize, ysize=ysize, /free, title=title
  differences = offsets[0, 0:*:2] - offsets[0, 1:*:2]
  ;diff_min = min(differences, max=diff_max)
  diff_min = -0.025
  diff_max = 0.025
  n_lt_min = total(differences lt diff_min, /integer)
  n_gt_max = total(differences gt diff_max, /integer)
  h = histogram(differences, min=diff_min, max=diff_max, $
                binsize=0.0005, locations=locs)
  plot, locs, h, $
        xstyle=1, xrange=[diff_min, diff_max], $
        psym=10, $
        color='000000'x, background='ffffff'x, $
        title='Distribution of beam 1 offset - beam 2 offset', charsize=charsize
  xyouts, 0.10, 0.8, /normal, $
          string(n_lt_min, diff_min, format='%d pts < %0.3f'), $
          charsize=1.1, color='000000'x
  xyouts, 0.90, 0.8, /normal, $
          string(n_gt_max, diff_max, format='%d pts > %0.3f'), $
          alignment=1.0, charsize=1.1, color='000000'x
  plots, fltarr(2) + offset_limit, !y.crange, color='0000ff'x

  title = string(file_basename(filename), $
                 format='(%"Distribution of H!D2!NO differences by beam for %s")')
  window, xsize=xsize, ysize=ysize, /free, title=title
  differences = offsets[1, 0:*:2] - offsets[1, 1:*:2]
  diff_min = min(differences, max=diff_max)
  h = histogram(differences, min=diff_min, max=diff_max, $
                binsize=0.02, locations=locs)
  plot, locs, h, $
        psym=10, $
        color='000000'x, background='ffffff'x, $
        charsize=charsize
end


; main-level example program

;eng_basedir = '/hao/twilight/Data/CoMP/engineering.continuum-correction-2014'
;basename = 'wave_cal_1074-2014.txt'

;eng_basedir = '/hao/twilight/Data/CoMP/engineering.continuum-correction-2017'
;basename = 'wave_cal_1074-2017.txt'
;basename = 'wave_cal_1079-2017.txt'

eng_basedir = '/hao/twilight/Data/CoMP/engineering.continuum-correction-2017-destray'
basename = 'wave_cal_1074-2017-destray.txt'
;basename = 'wave_cal_1079-2017-destray.txt'

eng_basedir = filepath('resource', subdir=['..'], root=mg_src_root())
basename = 'wave_cal_1074_2.txt'

filename = filepath(basename, root=eng_basedir)
comp_plot_continuum_corrections, filename

end
