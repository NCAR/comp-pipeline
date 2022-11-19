; docformat = 'rst'

pro comp_intensity_timeseries, wave_region, process_basedir, average=average
  compile_opt strictarr
  @comp_mask_constants_common

  if (n_elements(average) gt 0L) then begin
    intensity_filename = string(wave_region, average, format='%s-%s-intensities.txt')
    background_filename = string(wave_region, average, format='%s-%s-backgrounds.txt')
    subdir = 'level2'
    glob = string(wave_region, average, format='????????.comp.%s.%s.synoptic.fts.gz')
  endif else begin
    intensity_filename = string(wave_region, format='%s-%sintensities.txt')
    background_filename = string(wave_region, format='%s-%sbackgrounds.txt')
    subdir = 'level1'
    glob = string(wave_region, format='????????.??????.comp.%s.*.*.fts.gz')
  endelse

  openw, intensity_lun, intensity_filename, /get_lun
  openw, background_lun, background_filename, /get_lun

  dates = file_basename(file_search(filepath('????????', root=process_basedir)))
  print, n_elements(dates), format='Found %d dates'
  for d = 0L, n_elements(dates) - 1L do begin
    comp_initialize, dates[d]

    full_glob = filepath(glob, $
                         subdir=[dates[d], subdir], $
                         root=process_basedir)
    print, full_glob
    files = file_search(full_glob, $
                        count=n_files)
                           
    for f = 0L, n_files - 1L do begin
      basename = file_basename(files[f])
      print, basename
      tokens = strsplit(basename, '.', /extract)
      lun = strpos(basename, 'bkg') ge 0 ? background_lun : intensity_lun

      primary_header = headfits(files[f])
      date_obs = string(sxpar(primary_header, 'DATE-OBS'), $
                       sxpar(primary_header, 'TIME-OBS'), $
                       format='(%"%sT%s")')
      date_obs = comp_normalize_datetime(date_obs)

      year   = long(strmid(date_obs,  0, 4))
      month  = long(strmid(date_obs,  5, 2))
      day    = long(strmid(date_obs,  8, 2))
      hour   = long(strmid(date_obs, 11, 2))
      minute = long(strmid(date_obs, 14, 2))
      second = long(strmid(date_obs, 17, 2))
      
      fhour = hour + minute / 60.0 + second / 60.0 / 60.0
      sun, year, month, day, fhour, sd=rsun, pa=pangle, la=bangle

      sun_pixels = rsun / plate_scale

      ; find center wavelength I extension
      n_waves = n_elements(average) gt 0L ? 5 : long(tokens[5])
      im = readfits(files[f], header, exten_no=n_waves / 2 + 1L, /silent)

      heights = [1.10, 1.15, 1.20, 1.25]
      means = dblarr(n_elements(heights))
      for h = 0L, n_elements(heights) - 1L do begin
        means[h] = comp_annulus_mean(im, $
                                     heights[h] - 0.02, $
                                     heights[h] + 0.02, $
                                     sun_pixels)
      endfor

      jd = julday(month, day, year, hour, minute, second)
      height_format = strarr(n_elements(heights)) + '%0.3f'
      printf, lun, jd, means, format='%0.8f ' + strjoin(height_format, ' ')
      print, strmid(basename, 0, 15), means, format='%s ' + strjoin(height_format, ' ')
    endfor
  endfor

  free_lun, intensity_lun, background_lun
end


; main-level example program

process_basedir = '/hao/dawn/Data/CoMP/process'

wave_regions = ['1074', '1079']
methods = ['mean', 'median']
for w = 0L, n_elements(wave_regions) - 1L do begin
  for m = 0L, n_elements(methods) - 1L do begin
    comp_intensity_timeseries, wave_regions[w], process_basedir, average=methods[m]
  endfor
endfor

end
