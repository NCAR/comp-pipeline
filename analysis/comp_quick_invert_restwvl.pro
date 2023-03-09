; docformat = 'rst'

pro comp_quick_invert_restwvl, wave_region, process_basedir
  compile_opt strictarr
  @comp_mask_constants_common
  nx = 620L
  ny = 620L

  output_filename = string(wave_region, format='%s-restwvl-2012.txt')
  openw, lun, output_filename, /get_lun

  glob = string(wave_region, format='????????.comp.%s.quick_invert.mean.synoptic.fts.gz')
  dates = file_basename(file_search(filepath('2012????', root=process_basedir)))
  print, n_elements(dates), format='Found %d dates'
  for d = 0L, n_elements(dates) - 1L do begin
    comp_initialize, dates[d]
    x = lindgen(nx)
    x = rebin(reform(x, nx, 1), nx, ny)

    full_glob = filepath(glob, $
                         subdir=[dates[d], 'level2'], $
                         root=process_basedir)
    files = file_search(full_glob, count=n_files)
    for f = 0L, n_files - 1L do begin
      basename = file_basename(files[f])
      print, basename
      tokens = strsplit(basename, '.', /extract)

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

      dop = readfits(files[f], header, exten_no=6L, /silent)

      good_dop_ind = where(finite(dop) and abs(dop) lt 80.0, n_good_dop)
      if (n_good_dop gt 0L) then begin
        median_rest_wavelength = median(dop[good_dop_ind])

        good_east_dop_ind = where(finite(dop) $
                                    and abs(dop) lt 80.0 $
                                    and x lt (nx - 1.0) / 2.0, n_good_east_dop)
        good_west_dop_ind = where(finite(dop) $
                                  and abs(dop) lt 80.0 $
                                  and x gt (nx - 1.0) / 2.0, n_good_west_dop)
        if (n_good_east_dop gt 0L) then begin
          east_median_rest_wavelength = median(dop[good_east_dop_ind])
        endif else east_median_rest_wavelength = !values.f_nan
        if (n_good_west_dop gt 0L) then begin
          west_median_rest_wavelength = median(dop[good_west_dop_ind])
        endif else west_median_rest_wavelength = !values.f_nan
      endif

      jd = julday(month, day, year, hour, minute, second)
      printf, lun, $
              jd, $
              median_rest_wavelength, $
              east_median_rest_wavelength, $
              west_median_rest_wavelength, $
              format='%0.8f   %0.3f   %0.3f   %0.3f'
    endfor
  endfor
  free_lun, lun
end


; main-level example program

process_basedir = '/hao/dawn/Data/CoMP/process'

wave_regions = ['1074', '1079']
for w = 0L, n_elements(wave_regions) - 1L do begin
  comp_quick_invert_restwvl, wave_regions[w], process_basedir
endfor

end
