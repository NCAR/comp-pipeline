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

  config_filename = filepath('comp.reprocess-check.cfg', $
                             subdir=['..', 'config'], $
                             root=mg_src_root())
  comp_configuration, config_filename=config_filename

  dirs = file_search(filepath('????????', root=process_basedir), count=n_dirs)
  n = 2000 * n_dirs
  good_times = dblarr(n) + !values.f_nan
  times = dblarr(n) + !values.f_nan
  good_bkgs = fltarr(n) + !values.f_nan
  bkgs = fltarr(n) + !values.f_nan
  dates = dblarr(n_dirs) + !values.f_nan
  median_bkgs = fltarr(n_dirs) + !values.f_nan
  median_good_bkgs = fltarr(n_dirs) + !values.f_nan
  count = 0L
  good_count = 0L
  for d = 0L, n_dirs - 1L do begin
    date = file_basename(dirs[d])
    gbu_basename = string(date, wave_region, format='(%"%s.comp.%s.gbu.log")')
    gbu_filename = filepath(gbu_basename, subdir='level1', root=dirs[d])
    if (file_test(gbu_filename, /regular)) then begin
      gbu = comp_read_gbu(gbu_filename, count=n_lines)

      date_parts = long(comp_decompose_date(date))
      dates[d] = julday(date_parts[1], date_parts[2], date_parts[0])

      median_bkgs[d] = median(gbu.background)
      jds = comp_plot_reprocess_bkgs_timeobs2jd(gbu.time_obs, times=day_times)

      bkgs[count:count + n_lines - 1] = gbu.background
      times[count:count + n_lines - 1] = jds

      good_indices = where(gbu.reason eq 0 and day_times lt 20.0, n_good_files)
      ;good_indices = where(day_times lt 20.0, n_good_files)
      if (n_good_files gt 0L) then begin
        median_good_bkgs[d] = median((gbu.background)[good_indices])
        good_times[good_count:good_count + n_good_files - 1] = jds[good_indices]
        good_bkgs[good_count:good_count + n_good_files - 1] = (gbu.background)[good_indices]
        good_count += n_good_files
      endif
      count += n_lines
    endif
  endfor
  times = times[0:count - 1]
  bkgs = bkgs[0:count - 1]
  dummy = label_date(date_format='%Y.%N.%D')
  charsize = 1.25
  yrange = [0.0, 20.0]
  xticks = 8
  window, xsize=1000, ysize=500, /free, title='Reprocess-check'
  plot, times, bkgs, $
        charsize=charsize, psym=3, color='000000'x, background='ffffff'x, $
        xtickformat='label_date', xtitle='Date', xstyle=1, xticks=xticks, $
        ytitle='Background', yrange=yrange, $
        title=string(wave_region, format='(%"Background for all %s nm files")')
  window, xsize=1000, ysize=500, /free, title='Reprocess-check'
  plot, good_times, good_bkgs, $
        charsize=charsize, psym=3, color='000000'x, background='ffffff'x, $
        xtickformat='label_date', xtitle='Date', xstyle=1, xticks=xticks, $
        ytitle='Background',  yrange=yrange, $
        title=string(wave_region, format='(%"Background for good %s nm files")')
  window, xsize=1000, ysize=500, /free, title='Reprocess-check'
  plot, dates, median_bkgs, $
        charsize=charsize, psym=2, symsize=0.75, color='000000'x, background='ffffff'x, $
        xtickformat='label_date', xtitle='Date', xstyle=1, xticks=xticks, $
        ytitle='Median background',  yrange=yrange, $
        title=string(wave_region, format='(%"Median background for all %s nm files")')
  window, xsize=1000, ysize=500, /free, title='Reprocess-check'
  plot, dates, median_good_bkgs, $
        charsize=charsize, psym=2, symsize=0.75, color='000000'x, background='ffffff'x, $
        xtickformat='label_date', xtitle='Date', xstyle=1, xticks=xticks, $
        ytitle='Median background',  yrange=yrange, $
        title=string(wave_region, format='(%"Median background for good %s nm files")')
end


; main-level example program

comp_plot_reprocess_bkgs, '1074'

end
