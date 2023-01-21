; docformat = 'rst'

;+
; Procedure to read CoMP Level_1 data files containing demodulated Stokes
; parameters and compute the mean, median and standard deviation of the like
; images in those files. This routine is able to handle a set of files to
; average which contain different wavelength and Stokes parameter images.
;
; Output:
;   YYYYMMDD.comp.WWWW.mean.TYPE.fts - file containing the mean of the input
;                                      images
;   YYYYMMDD.comp.WWWW.median.TYPE.fts - file containing the median of the
;                                        input images
;   YYYYMMDD.comp.WWWW.sigma.TYPE.fts - file containing the standard deviation
;                                       of the input images where wwww is the
;                                       wave_type
;
; :Todo:
;   - replace all the loops and statistics with the moment() function, 
;  which has the dimension parameter. See comp_find_systematics for an example
; (LS)
;
;   - modify routine to optionally input coaligned .ca files (ST)
;
;   - modify routine to optionally input and output compressed files (ST)
;
; :Examples:
;   For example, calling it like::
;
;     comp_average, '20110316', '1074', list_file='files.txt'
;
;   will use filename 'files.txt' as input file. The following::
;
;     comp_average, '20110902', '1074', /synoptic
;
;   will use filename '20110902.comp.1074.good.synoptic.files.txt' as input
;   filename. Finally::
;
;     comp_average, '20131026', '1074'
;
;   will use '20131026.comp.1074.good.iqu.files.txt' as input filename.
;
; :Uses:
;   comp_config_common, comp_constants_common, comp_inventory_l2, comp_uniq,
;   comp_l2_mask, comp_find_average_files, comp_find_l1_file, fits_open,
;   fits_read, fits_write, fits_close, sxdelpar, sxaddpar, mg_log
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :Keywords:
;   synoptic : in, optional, type=boolean
;     set to use synoptic file instead of waves file
;   found_files : out, optional, type=long
;     set to a named variable to return whether files were found to produce an
;     average
;   error : out, optional, type=long
;     set to a named variable to return the error status of the routine, 0 for
;     success, anything else for failure
;
; :Author:
;   MLSO Software Team
;
; :History:
;   Removed limit of first 50 files, 6/16/14, ST
;   removed gzip                   Oct 1 2014    GdT
;   changed DATE_OBS to DATE_HST   Oct 2 2014    GdT
;   changed TIME_OBS to TIME_HST   Oct 2 2014    GdT
;   see git log for recent changes
;-
pro comp_average, date_dir, wave_type, $
                  synoptic=synoptic, $
                  found_files=found_files, $
                  error=error
  compile_opt idl2
  @comp_config_common
  @comp_constants_common

  mg_log, 'wave type: %s, files: %s', wave_type, $
          keyword_set(synoptic) ? 'synoptic' : 'waves', $
          name='comp', /info

  found_files = 0B
  catch, error
  if (error ne 0) then begin
    catch, /cancel
    mg_log, /last_error, name='comp'
    goto, done
  endif

  l1_process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)
  l2_process_dir = filepath('', subdir=[date_dir, 'level2'], root=process_basedir)
  cd, l2_process_dir

  ; calibration mode if averaging backgrounds by polarization
  calibration = average_background_by_polarization

  ; find the files to average
  i_files = comp_find_average_files(date_dir, wave_type, $
                                    stokes_present=stokes_present, $
                                    count=n_files, $
                                    calibration=calibration, $
                                    synoptic=synoptic, $
                                    qu_files=qu_files, $
                                    v_files=v_files)

  found_files = n_files gt 0L
  if (~found_files) then begin
    mg_log, 'no good %s files for %s, exiting', $
            wave_type, $
            keyword_set(synoptic) ? 'synoptic' : 'waves', $
            name='comp', /warn
    goto, done
  endif

  n_i_files  = n_elements(i_files)
  n_qu_files = n_elements(qu_files)
  n_v_files  = n_elements(v_files)

  mg_log, 'averaging %d files: %d I, %d QU, %d V', $
          n_files, n_i_files, n_qu_files, n_v_files, $
          name='comp', /info

  ; time period for this averaging
  year     = long(strmid(i_files[0], 0, 4))
  month    = long(strmid(i_files[0], 4, 2))
  day      = long(strmid(i_files[0], 6, 2))
  hour     = long(strmid(i_files[0], 9, 2))
  minute   = long(strmid(i_files[0], 11, 2))
  second   = long(strmid(i_files[0], 13, 2))
  start_jd = julday(month, day, year, hour, minute, second)

  year    = long(strmid(i_files[n_i_files - 1], 0, 4))
  month   = long(strmid(i_files[n_i_files - 1], 4, 2))
  day     = long(strmid(i_files[n_i_files - 1], 6, 2))
  hour    = long(strmid(i_files[n_i_files - 1], 9, 2))
  minute  = long(strmid(i_files[n_i_files - 1], 11, 2))
  second  = long(strmid(i_files[n_i_files - 1], 13, 2))
  end_jd = julday(month, day, year, hour, minute, second)

  duration = end_jd - start_jd
  mid_jd = start_jd + duration / 2
  caldat, mid_jd, utmonth, utday, utyear, uthour, utminute, utsecond

  utyear   = string(utyear, format='(I4)')
  utday    = string(utday, format='(I02)')
  utmonth  = string(utmonth, format='(I02)')
  uthour   = string(uthour, format='(I02)')
  utminute = string(utminute, format='(I02)')
  utsecond = string(round(utsecond), format='(I02)')

  ; construct FITS standard strings
  date_str = utyear + '-' + utmonth + '-' + utday
  time_str = uthour + ':' + utminute + ':' + utsecond

  type = keyword_set(synoptic) ? 'synoptic' : 'waves'

  if (compute_mean) then begin
    mean_filename = date_dir + '.comp.' + wave_type + '.mean.' + type + '.fts'
    fits_open, mean_filename, fcbavg, /write
  endif

  if (compute_median) then begin
    median_filename = date_dir + '.comp.' + wave_type + '.median.' + type + '.fts'
    fits_open, median_filename, fcbmed, /write
  endif

  sigma_filename = date_dir + '.comp.' + wave_type + '.sigma.' + type + '.fts'
  fits_open, sigma_filename, fcbsig, /write

  ; use test file to get sample headers
  test_filename = filepath(i_files[0], root=l1_process_dir)
  fits_open, test_filename, fcb

  ; read the primary header to use for the output
  fits_read, fcb, d, average_primary_header, /header_only, exten_no=0, $
             /no_abort, message=msg
  if (msg ne '') then begin
    mg_log, 'problem reading from %s', test_filename, name='comp', /error
    fits_close, fcb
    fits_close, fcbavg
    fits_close, fcbmed
    fits_close, fcbsig
    message, msg
  endif
  sxdelpar, average_primary_header, 'DATE_HST'
  sxdelpar, average_primary_header, 'TIME_HST'
  sxdelpar, average_primary_header, 'METHOD'
  sxaddpar, average_primary_header, 'LEVEL   ', 'L2'
  fits_close, fcb

  sxaddpar, average_primary_header, 'DATE-OBS', date_str, $
            ' [UTC] Averaging mid-point DATE: CCYY-MM-DD', after='TIMESYS'
  sxaddpar, average_primary_header, 'TIME-OBS', time_str, $
            ' [UTC] Averaging mid-point TIME: HH:MM:SS', after='DATE-OBS'
  sxaddpar, average_primary_header, 'DURATION', 24.0 * 60.0 * duration, $
            ' [minutes] Averaging duration', after='TIME-OBS', format='(f8.3)'

  sxdelpar, average_primary_header, 'OBS_PLAN'
  sxdelpar, average_primary_header, 'OBS_ID'

  ; use given 5-pt wavelengths
  case wave_type of
    '1074': waves = wavelengths_5pt_1074
    '1079': waves = wavelengths_5pt_1079
    else: begin
        mg_log, 'no 5-point reference wavelengths of wave type %s', wave_type, $
                name='comp', /error
        goto, done
      end
  endcase

  n_waves = n_elements(waves)
  sxaddpar, average_primary_header, 'NTUNES', keyword_set(synoptic) ? 5 : 3

  keywords_to_average = ['IXCNTER1', 'IYCNTER1', 'IRADIUS1', $
                         'IXCNTER2', 'IYCNTER2', 'IRADIUS2', $
                         'OXCNTRU1', 'OYCNTRU1', 'ORADU1', $
                         'OXCNTRU2', 'OYCNTRU2', 'ORADUS2', $
                         'FRADIUS', 'FRPIX1', 'FRPIX2']
  keyword_values = fltarr(n_elements(keywords_to_average), n_i_files)
  for f = 0L, n_i_files - 1L do begin
    fits_open, filepath(i_files[f], root=l1_process_dir), fcb
    fits_read, fcb, d, theader, /header_only, exten_no=0, $
               /no_abort, message=msg
    if (msg ne '') then begin
      mg_log, 'problem reading from %s', filename, name='comp', /error
      message, msg
    endif

    for k = 0L, n_elements(keywords_to_average) - 1L do begin
      keyword_values[k, f] = sxpar(theader, keywords_to_average[k])
    endfor
    fits_close, fcb
  endfor

  average_keyword_values = median(keyword_values, dimension=2)
  for k = 0L, n_elements(keywords_to_average) - 1L do begin
    !null = where(abs(keyword_values[k, *] - average_keyword_values[k]) gt 0.2 * average_keyword_values[k], $
                  n_outliers)
    if (n_outliers gt 0L) then begin
      mg_log, '%d files differ by > 20%% for %s', $
              n_outliers, keywords_to_average[k], $
              name='comp', /warn
    endif
    sxaddpar, average_primary_header, keywords_to_average[k], average_keyword_values[k]
  endfor

  comp_l2_update_version, average_primary_header

  if (compute_median) then fits_write, fcbmed, 0, average_primary_header
  if (compute_mean) then fits_write, fcbavg, 0, average_primary_header
  fits_write, fcbsig, 0, average_primary_header

  mg_log, '%s', strjoin(strtrim(waves, 2), ', '), name='comp', /debug

  ; compute averages
  if (keyword_set(average_background_by_polarization)) then begin
    back = fltarr(nx, ny, n_stokes, n_waves)
  endif else begin
    back = fltarr(nx, ny, n_waves)
  endelse

  ; summation of number of files going into average
  naverage = lonarr(n_stokes, n_waves)
  num_averaged = lonarr(n_stokes, n_waves)

  ; background average depends on BACKGROUND_BY_POLARIZATION keyword
  back_naverage = keyword_set(average_background_by_polarization) $
                    ? lonarr(n_stokes, n_waves) $
                    : lonarr(n_waves)
  num_back_averaged = keyword_set(average_background_by_polarization) $
                        ? lonarr(n_stokes, n_waves) $
                        : lonarr(n_waves)

  average_times = strarr(2, n_stokes, n_waves)

  for s = 0L, n_stokes - 1L do begin
    mg_log, 'starting averaging for %s', stokes[s], name='comp', /debug
    case strlowcase(stokes[s]) of
      'i': s_files = i_files
      'q': s_files = qu_files
      'u': s_files = qu_files
      'v': s_files = v_files
    endcase

    n_s_files = n_elements(s_files)

    ; quit if no files for this Stokes parameter
    if (n_s_files eq 0) then begin
      mg_log, 'no files, skipping %s', stokes[s], name='comp', /debug
      continue
    endif else begin
      mg_log, '%d %s files', n_s_files, stokes[s], name='comp', /debug
    endelse

    for w = 0L, n_waves - 1L do begin
      mg_log, 'Stokes %s wave %0.2f', stokes[s], waves[w], $
              name='comp', /debug

      ; REFORM to make sure IDL doesn't drop a dimension of size 1
      data = reform(fltarr(nx, ny, n_s_files), $
                    nx, ny, n_s_files)

      header = !null
      for f = 0L, n_s_files - 1L do begin
        filename = s_files[f]
        name = strmid(filename, 0, 15)
        mg_log, 'file %d/%d for %s @ %s: %s', $
                f + 1, $
                n_s_files, $
                strtrim(stokes[s], 2), $
                strtrim(waves[w], 2), $
                name, $
                name='comp', /debug

        fits_open, filepath(filename, root=l1_process_dir), fcb
        fits_read, fcb, d, theader, /header_only, exten_no=0, $
                   /no_abort, message=msg
        if (msg ne '') then begin
          mg_log, 'problem reading from %s', filename, name='comp', /error
          message, msg
        endif
        mask = comp_l2_mask(theader)

        comp_inventory_l1, fcb, wave, pol

        good = where(pol eq stokes[s] and wave eq waves[w], count)
        if (count eq 0) then begin
          fits_close, fcb
          continue
        endif
        fits_read, fcb, dat, header, exten_no=good[0] + 1, $
                   /no_abort, message=msg
        if (msg ne '') then begin
          mg_log, 'problem reading from %s', filename, name='comp', /error
          message, msg
        endif
        naverage[s, w] += sxpar(header, 'NAVERAGE')

        ; put NaNs for masked out pixels, so averaging doesn't include a
        ; bunch of 0's
        bad_ind = where(mask eq 0.0, n_bad)
        if (n_bad gt 0L) then mask[bad_ind] = !values.f_nan

        data[*, *, f] = dat * mask
        if (num_averaged[s, w] eq 0) then average_times[0, s, w] = strmid(name, 9, 6)
        num_averaged[s, w] += 1
        average_times[1, s, w] = strmid(name, 9, 6)

        ; sum background images first time through
        background_filename = comp_find_l1_file(date_dir, wave_type, $
                                                datetime=name, $
                                                /background)

        if (keyword_set(average_background_by_polarization)) then begin
          fits_open, background_filename, bkg_fcb
          fits_read, bkg_fcb, dat, bkg_header, $
                     extname=string(stokes[s], waves[w], format='(%"BKG%s, %0.2f")'), $
                     /no_abort, message=msg
          if (msg ne '') then begin
            mg_log, 'problem reading from %s', filename, name='comp', /error
            message, msg
          endif

          back[*, *, s, w] += dat * mask
          back_naverage[s, w] += sxpar(bkg_header, 'NAVERAGE')
          num_back_averaged[s, w] += 1
          fits_close, bkg_fcb
        endif else if (s eq 0) then begin
          fits_open, background_filename, bkg_fcb
          fits_read, bkg_fcb, dat, bkg_header, $
                     extname=string(stokes[s], waves[w], format='(%"BKG%s, %0.2f")'), $
                     /no_abort, message=msg
          if (msg ne '') then begin
            mg_log, 'problem reading from %s', filename, name='comp', /error
            message, msg
          endif

          back[*, *, w] += dat * mask
          back_naverage[w] += sxpar(bkg_header, 'NAVERAGE')
          num_back_averaged[w] += 1
          fits_close, bkg_fcb
        endif

        fits_close, fcb
      endfor

      ; TODO: create primary_header, headers and back_headers

      ; TODO: comp_update_polarimetric_correction, primary_header, data, headers

      ; TODO: may have to reform back to be (x, y, n_images) and then back
      ; TODO: comp_update_polarimetric_correction, primary_header, back, back_headers

      sxaddpar, header, 'LEVEL   ', 'L2'
      ename = stokes[s] + ', ' + string(format='(f7.2)', waves[w])

      ; calculate noise sigma

      ; format times
      hrs  = strmid(average_times[*, s, w], 0, 2)
      mins = strmid(average_times[*, s, w], 2, 2)
      secs = strmid(average_times[*, s, w], 4, 2)
      average_times[*, s, w] = hrs + ':' + mins + ':' + secs

      sm = sqrt(n_s_files)

      sxaddpar, header, 'POLSTATE', stokes[s]
      sxaddpar, header, 'WAVELENG', waves[w]
      sxaddpar, header, 'NAVERAGE', n_s_files
      sxaddpar, header, 'NFILES', num_averaged[s, w], ' Number of files used', $
                after='NAVERAGE'

      start_time = num_averaged[s, w] eq 0L ? 0L : average_times[0, s, w]
      end_time   = num_averaged[s, w] eq 0L ? 0L : average_times[1, s, w]

      sxaddpar, header, 'AVESTART', start_time, $
                ' [UTC] Start of averaging HH:MM:SS', after='NAVERAGE'
      sxaddpar, header, 'AVEEND', end_time, $
                ' [UTC] End of averaging HH:MM:SS', after='AVESTART'

      sigma = fltarr(nx, ny)
      for x = 0L, nx - 1L do begin
        for y = 0L, ny - 1L do begin
          ; use /NAN keyword to not count masked pixels
          sumx = total(data[x, y, *], /nan)
          sumx2 = total(data[x, y, *]^2, /nan)

          xbar = sumx / n_s_files
          var = (sumx2 - 2.0 * xbar * sumx + n_s_files * xbar^2) / (n_s_files - 1.0)

          ; TODO: ?
          ;sigma[x, y] = sqrt(var) / sm
          sigma[x, y] = sqrt(var)
        endfor
      endfor

      ; write sigma parameters to output files
      if (num_averaged[s, w] gt 0L) then begin
        fits_write, fcbsig, sigma, header, extname=ename
      endif

      ; find median and mean across image
      sxaddpar, header, 'NAVERAGE', naverage[s, w]

      ; dimension 3 goes away if there is only 1 image to average
      data = reform(data)
      if (size(data, /n_dimensions) lt 3) then begin
        med = data
        aver = data
      endif else begin
        med = median(data, dimension=3)
        aver = mean(data, dimension=3, /nan)   ; use /NAN to not use masked pixels
      endelse

      ; write Stokes parameters to output files
      if (compute_median) then begin
        sxaddpar, header, 'DATAMIN', min(med, /nan), ' minimum data value', format='(F0.3)'
        sxaddpar, header, 'DATAMAX', max(med, /nan), ' maximum data value', format='(F0.3)'

        if (num_averaged[s, w] ne 0L) then begin
          mg_log, 'writing median...', name='comp', /debug
          fits_write, fcbmed, med, header, extname=ename
        endif
      endif

      if (compute_mean) then begin
        sxaddpar, header, 'DATAMIN', min(aver, /nan), ' minimum data value', format='(F0.3)'
        sxaddpar, header, 'DATAMAX', max(aver, /nan), ' maximum data value', format='(F0.3)'

        if (num_averaged[s, w] ne 0L) then begin
          mg_log, 'writing mean...', name='comp', /debug
          fits_write, fcbavg, aver, header, extname=ename
        endif
      endif

      mg_log, 'finished averaging %0.2f for %s', waves[w], stokes[s], $
              name='comp', /debug
    endfor
    mg_log, 'finished averaging %s', stokes[s], name='comp', /debug
  endfor

  ; write mean background image to output files
  mg_log, 'writing background averages...', name='comp', /debug
  if (keyword_set(average_background_by_polarization)) then begin
    for s = 0L, n_stokes - 1L do begin
      if (numof_stokes[s] eq 0) then continue

      for w = 0L, n_waves - 1L do begin
        if (num_back_averaged[s, w] eq 0) then continue
        back[*, *, s, w] /= float(num_back_averaged[s, w])

        sxaddpar, header, 'WAVELENG', waves[w], ' [nm] Wavelength of obs'
        sxaddpar, header, 'NAVERAGE', back_naverage[s, w]
        sxaddpar, header, 'NFILES', num_back_averaged[s, w], $
                  ' Number of files used', $
                  after='NAVERAGE'
        sxaddpar, header, 'DATAMIN', min(back[*, *, s, w], /nan), $
                  ' minimum data value', format='(F0.3)'
        sxaddpar, header, 'DATAMAX', max(back[*, *, s, w], /nan), $
                  ' maximum data value', format='(F0.3)'
        sxaddpar, header, 'POLSTATE', string(stokes[s], format='(%"BKG%s")')
        ename = string(stokes[s], waves[w], format='(%"B%s, %7.2f")')
        if (compute_median) then begin
          fits_write, fcbmed, back[*, *, s, w], header, extname=ename
        endif
        if (compute_mean) then begin
          fits_write, fcbavg, back[*, *, s, w], header, extname=ename
        endif
      endfor
    endfor
  endif else begin
    for w = 0L, n_waves - 1L do begin
      if (num_back_averaged[w] eq 0) then continue
      back[*, *, w] /= float(num_back_averaged[w])

      sxaddpar, header, 'WAVELENG', waves[w], ' [nm] Wavelength of obs'
      sxaddpar, header, 'NAVERAGE', back_naverage[w]
      sxaddpar, header, 'NFILES', num_back_averaged[w], $
                ' Number of files used', $
                after='NAVERAGE'
      sxaddpar, header, 'DATAMIN', min(back[*, *, w], /nan), $
                ' minimum data value', format='(F0.3)'
      sxaddpar, header, 'DATAMAX', max(back[*, *, w], /nan), $
                ' maximum data value', format='(F0.3)'
      sxaddpar, header, 'POLSTATE', 'BKG'
      ename = 'B, ' + string(waves[w], format='(f7.2)')

      if (compute_median) then begin
        fits_write, fcbmed, back[*, *, w], header, extname=ename
      endif

      if (compute_mean) then begin
        fits_write, fcbavg, back[*, *, w], header, extname=ename
      endif
    endfor
  endelse

  ; close files
  if (compute_median) then begin
    fits_close, fcbmed

    zip_cmd = string(median_filename, format='(%"gzip -f %s")')
    spawn, zip_cmd, result, error_result, exit_status=status
    if (status ne 0L) then begin
      mg_log, 'problem zipping median file with command: %s', zip_cmd, $
              name='comp', /error
      mg_log, '%s', error_result, name='comp', /error
    endif
  endif

  if (compute_mean) then begin
    fits_close, fcbavg

    zip_cmd = string(mean_filename, format='(%"gzip -f %s")')
    spawn, zip_cmd, result, error_result, exit_status=status
    if (status ne 0L) then begin
      mg_log, 'problem zipping mean file with command: %s', zip_cmd, $
              name='comp', /error
      mg_log, '%s', error_result, name='comp', /error
    endif
  endif

  fits_close, fcbsig

  zip_cmd = string(sigma_filename, format='(%"gzip -f %s")')
  spawn, zip_cmd, result, error_result, exit_status=status
  if (status ne 0L) then begin
    mg_log, 'problem zipping sigma file with command: %s', zip_cmd, $
            name='comp', /error
    mg_log, '%s', error_result, name='comp', /error
  endif

  done:
  fits_close, fcb
  fits_close, bkg_fcb
  fits_close, fcbmed
  fits_close, fcbavg
  fits_close, fcbsig

  mg_log, 'done', name='comp', /info
end


; main-level example program

date = '20180104'
wave_types = ['1074', '1079', '1083']

config_basename = 'comp.mgalloy.mahi.latest.cfg'
config_filename = filepath(config_basename, $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())

comp_configuration, config_filename=config_filename
comp_initialize, date

comp_setup_loggers
comp_setup_loggers_date, date
comp_setup_loggers_date, date, /rotate
comp_setup_loggers_eng, date

for w = 0L, n_elements(wave_types) - 1L do begin
  comp_average, date, wave_types[w], $
                error=error, found_files=waves_files_found
  print, wave_types[w], waves_files_found ? '' : 'not ', $
         format='(%"%s nm waves files %sfound")'

  comp_average, date, wave_types[w], /synoptic, $
                error=error, found_files=synoptic_files_found
  print, wave_types[w], synoptic_files_found ? '' : 'not ', $
         format='(%"%s nm synoptic %sfiles found")'
endfor

end
