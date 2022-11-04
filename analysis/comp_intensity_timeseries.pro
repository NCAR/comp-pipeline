; docformat = 'rst'

pro comp_intensity_timeseries, wave_region, process_basedir
  compile_opt strictarr
  @comp_mask_constants_common

  intensity_filename = string(wave_region, format='%s-intensities.txt')
  background_filename = string(wave_region, format='%s-backgrounds.txt')

  openw, intensity_lun, intensity_filename, /get_lun
  openw, background_lun, background_filename, /get_lun

  dates = file_basename(file_search(filepath('????????', root=process_basedir)))
  for d = 0L, n_elements(dates) - 1L do begin
    comp_initialize, dates[d]

    l1_files = file_search(filepath(string(wave_region, format='????????.??????.comp.%s.*.*.fts.gz'), $
                           subdir=[dates[d], 'level1'], $
                           root=process_basedir), count=n_l1_files)
                           
    for f = 0L, n_l1_files - 1L do begin
      basename = file_basename(l1_files[f])
      tokens = strsplit(basename, '.', /extract)
      lun = strpos(basename, 'bkg') ge 0 ? background_lun : intensity_lun

      primary_header = headfits(l1_files[f])
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
      n_waves = long(tokens[5])
      im = readfits(l1_files[f], header, exten_no=n_waves / 2 + 1L, /silent)

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
for w = 0L, n_elements(wave_regions) - 1L do begin
  comp_intensity_timeseries, wave_regions[w], process_basedir
endfor

end
