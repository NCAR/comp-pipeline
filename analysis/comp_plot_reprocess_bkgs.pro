; docformat = 'rst'

function comp_plot_reprocess_bkgs_timeobs2jd, time_obs, times=times
  compile_opt strictarr

  ; 2017-12-14 02:17:06
  year = long(strmid(time_obs, 0, 4))
  month = long(strmid(time_obs, 5, 2))
  day = long(strmid(time_obs, 8, 2))
  hour = long(strmid(time_obs, 11, 2))
  minute = long(strmid(time_obs, 14, 2))
  second = long(strmid(time_obs, 17, 2))
  times = hour + (minute + second / 60.0) / 60.0
  jd = julday(month, day, year, hour, minute, second)
  return, jd
end


pro comp_plot_reprocess_bkgs, wave_region
  compile_opt strictarr
  @comp_config_common

  flags = 'reprocess'
  config_filename = filepath(string(flags, format='(%"comp.%s.cfg")'), $
                             subdir=['..', 'config'], $
                             root=mg_src_root())
  comp_configuration, config_filename=config_filename

  dirs = file_search(filepath('????????', root=process_basedir), count=n_dirs)
  n = 2000 * n_dirs
  good_times = dblarr(n) + !values.f_nan
  times = dblarr(n) + !values.f_nan
  good_bkgs = fltarr(n) + !values.f_nan
  good_sigma = fltarr(n) + !values.f_nan
  bkgs = fltarr(n) + !values.f_nan
  sigma = fltarr(n) + !values.f_nan
  dates = dblarr(n_dirs) + !values.f_nan
  median_bkgs = fltarr(n_dirs) + !values.f_nan
  median_sigma = fltarr(n_dirs) + !values.f_nan
  median_good_bkgs = fltarr(n_dirs) + !values.f_nan
  median_good_sigma = fltarr(n_dirs) + !values.f_nan

  count = 0L
  good_count = 0L
  for d = 0L, n_dirs - 1L do begin
    date = file_basename(dirs[d])
    print, d + 1, n_dirs, date, format='(%"%03d/%03d: %s")'
    gbu_basename = string(date, wave_region, format='(%"%s.comp.%s.gbu.log")')
    gbu_filename = filepath(gbu_basename, subdir='level1', root=dirs[d])
    if (file_test(gbu_filename, /regular)) then begin
      gbu = comp_read_gbu(gbu_filename, count=n_lines)

      date_parts = long(comp_decompose_date(date))
      dates[d] = julday(date_parts[1], date_parts[2], date_parts[0])

      median_bkgs[d] = median(gbu.background)
      median_sigma[d] = median(gbu.variance)

      jds = comp_plot_reprocess_bkgs_timeobs2jd(gbu.time_obs, times=day_times)

      bkgs[count:count + n_lines - 1] = gbu.background
      sigma[count:count + n_lines - 1] = gbu.variance
      times[count:count + n_lines - 1] = jds

      good_indices = where(gbu.reason eq 0 and day_times lt 20.0, n_good_files)
      ;good_indices = where(day_times lt 20.0, n_good_files)
      if (n_good_files gt 0L) then begin
        median_good_bkgs[d] = median((gbu.background)[good_indices])
        median_good_sigma[d] = median((gbu.variance)[good_indices])
        ; print, date, median_good_bkgs[d], median_bkgs[d], $
        ;        format='(%"%d: median good bkg=%0.3f, median bkg=%0.3f")'
        good_times[good_count:good_count + n_good_files - 1] = jds[good_indices]
        good_bkgs[good_count:good_count + n_good_files - 1] = (gbu.background)[good_indices]
        good_sigma[good_count:good_count + n_good_files - 1] = (gbu.variance)[good_indices]
        good_count += n_good_files
      endif
      count += n_lines
    endif
  endfor

  jd1 = julday(9, 15, 2013)
  jd2 = julday(1, 1, 2016)

  times = times[0:count - 1]
  bkgs = bkgs[0:count - 1]
  dummy = label_date(date_format='%Y.%N.%D')
  charsize = 1.25
  yrange = [0.0, 20.0]
  xticks = 8
  color = '000000'x
  background = 'ffffff'x
  annotation_color = 'a0a0a0'x
  variance_yrange = [0.0, 3.0]

  show_threshold = 0B

  window, xsize=1000, ysize=500, /free, title=flags
  plot, times, bkgs, $
        charsize=charsize, psym=3, color=color, background=background, $
        xtickformat='label_date', xtitle='Date', xstyle=1, xticks=xticks, $
        ytitle='Background', yrange=yrange, $
        title=string(wave_region, format='(%"Background for all %s nm files")')
  if (keyword_set(show_threshold)) then begin
    oplot, dblarr(2) + jd1, !y.crange, color=annotation_color
    oplot, dblarr(2) + jd2, !y.crange, color=annotation_color
  endif

  window, xsize=1000, ysize=500, /free, title=flags
  plot, times, sigma, $
        charsize=charsize, psym=3, color=color, background=background, $
        xtickformat='label_date', xtitle='Date', xstyle=1, xticks=xticks, $
        ytitle='Variance', yrange=variance_yrange, $
        title=string(wave_region, format='(%"Variance for all %s nm files")')
      
  window, xsize=1000, ysize=500, /free, title=flags
  plot, good_times, good_bkgs, $
        charsize=charsize, psym=3, color=color, background=background, $
        xtickformat='label_date', xtitle='Date', xstyle=1, xticks=xticks, $
        ytitle='Background',  yrange=yrange, $
        title=string(wave_region, format='(%"Background for good %s nm files")')
  if (keyword_set(show_threshold)) then begin
    oplot, dblarr(2) + jd1, !y.crange, color=annotation_color
    oplot, dblarr(2) + jd2, !y.crange, color=annotation_color
  endif

  window, xsize=1000, ysize=500, /free, title=flags
  plot, good_times, good_sigma, $
        charsize=charsize, psym=3, color=color, background=background, $
        xtickformat='label_date', xtitle='Date', xstyle=1, xticks=xticks, $
        ytitle='Variance',  yrange=variance_yrange, $
        title=string(wave_region, format='(%"Variance for good %s nm files")')

  window, xsize=1000, ysize=500, /free, title=flags
  plot, dates, median_bkgs, $
        charsize=charsize, psym=2, symsize=0.75, color=color, background=background, $
        xtickformat='label_date', xtitle='Date', xstyle=1, xticks=xticks, $
        ytitle='Median background',  yrange=yrange, $
        title=string(wave_region, format='(%"Median background for all %s nm files")')
  if (keyword_set(show_threshold)) then begin
    oplot, dblarr(2) + jd1, !y.crange, color=annotation_color
    oplot, dblarr(2) + jd2, !y.crange, color=annotation_color
  endif

  window, xsize=1000, ysize=500, /free, title=flags
  plot, dates, median_sigma, $
        charsize=charsize, psym=2, symsize=0.75, color=color, background=background, $
        xtickformat='label_date', xtitle='Date', xstyle=1, xticks=xticks, $
        ytitle='Median variance',  yrange=variance_yrange, $
        title=string(wave_region, format='(%"Median variance for all %s nm files")')

  window, xsize=1000, ysize=500, /free, title=flags
  plot, dates, median_good_bkgs, $
        charsize=charsize, psym=2, symsize=0.75, color=color, background=background, $
        xtickformat='label_date', xtitle='Date', xstyle=1, xticks=xticks, $
        ytitle='Median background',  yrange=yrange, $
        title=string(wave_region, format='(%"Median background for good %s nm files")')
  if (keyword_set(show_threshold)) then begin
    oplot, dblarr(2) + jd1, !y.crange, color=annotation_color
    oplot, dblarr(2) + jd2, !y.crange, color=annotation_color
  endif

  window, xsize=1000, ysize=500, /free, title=flags
  plot, dates, median_good_sigma, $
        charsize=charsize, psym=2, symsize=0.75, color=color, background=background, $
        xtickformat='label_date', xtitle='Date', xstyle=1, xticks=xticks, $
        ytitle='Median variance',  yrange=variance_yrange, $
        title=string(wave_region, format='(%"Median variance for good %s nm files")')

  if (keyword_set(show_threshold)) then begin
    epoch1_indices = where(times lt jd1, n_epoch1)
    epoch2_indices = where(times gt jd1 and times lt jd2, n_epoch2)
    epoch3_indices = where(times gt jd2, n_epoch3)

    h = histogram(bkgs[epoch1_indices], binsize=0.1, min=0.0, max=60.0, locations=locs)
    ch = total(h, /cumulative) / total(h)
    window, xsize=1000, ysize=500, /free, title=flags
    plot, locs, 100.0 * ch, $
          charsize=charsize, psym=10, color=color, background=background, $
          xtitle='Background threshold', xrange=[0.0, 20.0], xstyle=1, $
          ytitle='% of images marked good', $
          title=string(label_date(0, 0, jd1, 1), format='Background threshold choice (before %s)')

    h = histogram(bkgs[epoch2_indices], binsize=0.1, min=0.0, max=60.0, locations=locs)
    ch = total(h, /cumulative) / total(h)
    window, xsize=1000, ysize=500, /free, title=flags
    plot, locs, 100.0 * ch, $
          charsize=charsize, psym=10, color=color, background=background, $
          xtitle='Background threshold', xrange=[0.0, 20.0], xstyle=1, $
          ytitle='% of images marked good', $
          title=string(label_date(0, 0, jd1, 1), label_date(0, 0, jd2, 1), $
                       format='Background threshold choice (between %s and %s)')

    h = histogram(bkgs[epoch3_indices], binsize=0.1, min=0.0, max=60.0, locations=locs)
    ch = total(h, /cumulative) / total(h)
    window, xsize=1000, ysize=500, /free, title=flags
    plot, locs, 100.0 * ch, $
          charsize=charsize, psym=10, color=color, background=background, $
          xtitle='Background threshold', xrange=[0.0, 20.0], xstyle=1, $
          ytitle='% of images marked good', $
          title=string(label_date(0, 0, jd2, 1), format='Background threshold choice (after %s)')
  endif
end


; main-level example program

comp_plot_reprocess_bkgs, '1074'

end
