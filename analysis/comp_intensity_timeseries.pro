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
    files = file_search(full_glob, $
                        count=n_files)
                           
    for f = 0L, n_files - 1L do begin
      basename = file_basename(files[f])
      tokens = strsplit(basename, '.', /extract)

      fits_open, files[f], fcb
      n_extensions = fcb.nextend
      fits_close, fcb

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
      n_waves = sxpar(primary_header, 'NTUNES')
      i1 = readfits(files[f], header1, exten_no=n_waves / 2, /silent)
      i2 = readfits(files[f], header2, exten_no=n_waves / 2 + 1L, /silent)
      i3 = readfits(files[f], header3, exten_no=n_waves / 2 + 2L, /silent)
      d_lambda = abs(sxpar(header1, 'WAVELENG') - sxpar(header2, 'WAVELENG'))
      comp_analytic_gauss_fit2, i1, i2, i3, d_lambda, dop, width, intensity

      n_polstates = n_extensions / n_waves
      bkg_ext = (n_polstates - 1L) * n_waves + n_waves / 2 + 1L
      background = readfits(files[f], header, exten_no=bkg_ext, /silent)

      heights = [1.06, 1.08, 1.10, 1.15, 1.20, 1.25]
      intensity_means = dblarr(n_elements(heights))
      background_means = dblarr(n_elements(heights))
      for h = 0L, n_elements(heights) - 1L do begin
        intensity_means[h] = comp_annulus_mean(intensity, $
                                     heights[h] - 0.02, $
                                     heights[h] + 0.02, $
                                     sun_pixels)
        background_means[h] = comp_annulus_mean(background, $
                                                heights[h] - 0.02, $
                                                heights[h] + 0.02, $
                                                sun_pixels)
      endfor

      jd = julday(month, day, year, hour, minute, second)
      height_format = strarr(n_elements(heights)) + '%0.3f'
      printf, intensity_lun, jd, intensity_means, format='%0.8f ' + strjoin(height_format, ' ')
      printf, background_lun, jd, background_means, format='%0.8f ' + strjoin(height_format, ' ')
      print, basename, intensity_means, background_means, $
             format='%s ' + strjoin(height_format, ' ') + ' ' + strjoin(height_format, ' ')
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
