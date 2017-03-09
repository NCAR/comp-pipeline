; docformat = 'rst'

;+
; Procedure to read CoMP Level_1 data files containing demodulated Stokes
; parameters and compute the mean, median and standard deviation of the like
; images in those files. This routine is able to handle a set of files to
; average which contain different wavelength and Stokes parameter images.
;
; Output:
;   comp.mean.wwww.fts - file containing the mean of the input images
;   comp.median.wwww.fts - file containing the median of the input images
;   comp.sigma.wwww.fts - file containing the standard deviation of the input
;     images where wwww is the wave_type
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
;   will use filename '20110902.synoptic.1074.files.txt' as input filename. Finally::
;
;     comp_average, '20131026', '1074'
;
;   will use '20131026.good.iqu.1074.files.txt' as input filename.
;
; :Uses:
;   comp_config_common, comp_constants_common, comp_inventory_l2, comp_uniq,
;   comp_make_mask, comp_find_average_files, comp_find_l1_file, fits_open,
;   fits_read, fits_write, fits_close, sxdelpar, sxaddpar, mg_log
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :Keywords:
;   error : out, optional, type=long
;     set to a named variable to return the error status of the routine, 0 for
;     success, anything else for failure
;
; :Author:
;   Tomczyk, Sitongia
;
; :History:
;   Removed limit of first 50 files, 6/16/14, ST
;   removed gzip                   Oct 1 2014    GdT
;   changed DATE_OBS to DATE_HST   Oct 2 2014    GdT
;   changed TIME_OBS to TIME_HST   Oct 2 2014    GdT
;-
pro comp_average, date_dir, wave_type, error=error
  compile_opt idl2
  @comp_config_common
  @comp_constants_common

  mg_log, 'wave_type: %s', wave_type, name='comp', /info

  catch, error
  if (error ne 0) then begin
    catch, /cancel
    mg_log, /last_error, name='comp'
    return
  endif

  l1_process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)
  l2_process_dir = filepath('', subdir=[date_dir, 'level2'], root=process_basedir)
  cd, l2_process_dir

  ; calibration mode if averaging backgrounds by polarization
  calibration = average_background_by_polarization

  ; find the files to average
  files = comp_find_average_files(date_dir, wave_type, $
                                  max_n_files=averaging_max_n_files, $
                                  min_n_cluster_files=averaging_min_n_cluster_files, $
                                  max_cadence_interval=averaging_max_cadence_interval, $
                                  max_n_noncluster_files=averaging_max_n_noncluster_files, $
                                  stokes_present=stokes_present, $
                                  count=n_files, $
                                  calibration=calibration)

  if (n_files lt 1) then begin
    mg_log, 'no good %s files, exiting', wave_type, name='comp', /warn
    return
  endif

  mg_log, 'averaging %d files: %s', n_files, strjoin(files, ', '), $
          name='comp', /info

  ; loop over filenames and determine number of each images for each Stokes
  ; parameter
  numof_stokes = intarr(n_stokes)
  which_file = intarr(n_files, n_stokes)
  filenames = strarr(n_files)

  for j = 0L, n_files - 1L do begin
    filenames[j] = strmid(files[j], 0, 15)
    for i = 0L, n_stokes - 1L do begin
      if (strpos(stokes_present[j], stokes[i]) gt -1) then begin
        which_file[numof_stokes[i], i] = j
        ++numof_stokes[i]
      endif
    endfor
  endfor

  ; time period for this averaging
  year     = long(strmid(filenames[0], 0, 4))
  month    = long(strmid(filenames[0], 4, 2))
  day      = long(strmid(filenames[0], 6, 2))
  hour     = long(strmid(filenames[0], 9, 2))
  minute   = long(strmid(filenames[0], 11, 2))
  second   = long(strmid(filenames[0], 13, 2))
  start_jd = julday(month, day, year, hour, minute, second)

  year    = long(strmid(filenames[n_files - 1], 0, 4))
  month   = long(strmid(filenames[n_files - 1], 4, 2))
  day     = long(strmid(filenames[n_files - 1], 6, 2))
  hour    = long(strmid(filenames[n_files - 1], 9, 2))
  minute  = long(strmid(filenames[n_files - 1], 11, 2))
  second  = long(strmid(filenames[n_files - 1], 13, 2))
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

  for i = 0L, n_stokes - 1L do begin
    mg_log, '%d image files for Stokes %s', numof_stokes[i], stokes[i], $
            name='comp', /info
  endfor

  if (compute_mean) then begin
    mean_filename = date_dir + '.comp.' + wave_type + '.mean.fts'
    fits_open, mean_filename, fcbavg, /write
  endif

  if (compute_median) then begin
    median_filename = date_dir + '.comp.' + wave_type + '.median.fts'
    fits_open, median_filename, fcbmed, /write
  endif

  sigma_filename = date_dir + '.comp.' + wave_type + '.sigma.fts'
  fits_open, sigma_filename, fcbsig, /write

  ; use test file to get sample headers
  test_filename = comp_find_l1_file(date_dir, wave_type, datetime=filenames[0])
  fits_open, test_filename, fcb

  ; read the primary header to use for the output
  fits_read, fcb, d, primary_header, /header_only, exten_no=0
  sxdelpar, primary_header, 'DATE_HST'
  sxdelpar, primary_header, 'TIME_HST'
  sxdelpar, primary_header, 'METHOD'
  sxaddpar, primary_header, 'LEVEL   ', 'L2'
  fits_close, fcb

  sxaddpar, primary_header, 'DATE-OBS', date_str, $
            ' [UTC] Averaging mid-point DATE: CCYY-MM-DD', after='TIMESYS'
  sxaddpar, primary_header, 'TIME-OBS', time_str, $
            ' [UTC] Averaging mid-point TIME: HH:MM:SS', after='DATE-OBS'
  sxaddpar, primary_header, 'DURATION', 24. * 60. * duration, $
            ' [minutes] Averaging duration', after='TIME-OBS', format='(f8.3)'

  sxdelpar, primary_header, 'OBS_PLAN'
  sxdelpar, primary_header, 'OBS_ID'

  ; use given 5-pt wavelengths
  case wave_type of
    '1074': waves = wavelengths_5pt_1074
    '1079': waves = wavelengths_5pt_1079
    else: begin
        mg_log, 'no 5-point reference wavelengths of wave type %s', wave_type, $
                name='comp', /error
        return
      end
  endcase

  n_waves = n_elements(waves)
  sxaddpar, primary_header, 'NTUNES', n_waves

  comp_l2_update_version, primary_header

  if (compute_median) then fits_write, fcbmed, 0, primary_header
  if (compute_mean) then fits_write, fcbavg, 0, primary_header
  fits_write, fcbsig, 0, primary_header

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

  for ist = 0L, n_stokes - 1L do begin
    if (numof_stokes[ist] eq 0) then continue
    for iw = 0L, n_waves - 1L do begin
      mg_log, 'Stokes %s wave %s', $
              strjoin(strtrim(stokes[ist], 2), ', '), $
              strjoin(strtrim(waves[iw], 2), ', '), $
              name='comp', /debug

      ; REFORM to make sure IDL doesn't drop a dimension of size 1
      data = reform(fltarr(nx, ny, numof_stokes[ist], /nozero), $
                    nx, ny, numof_stokes[ist])

      header = !null
      for ifile = 0L, numof_stokes[ist] - 1L do begin
        name = filenames[which_file[ifile, ist]]
        filename = comp_find_l1_file(date_dir, wave_type, datetime=name)
        mg_log, 'file %d/%d for %s @ %s: %s', $
                ifile + 1, $
                numof_stokes[ist], $
                strtrim(stokes[ist], 2), $
                strtrim(waves[iw], 2), $
                name, $
                name='comp', /debug

        fits_open, filename, fcb
        fits_read, fcb, d, theader, /header_only, exten_no=0
        comp_make_mask, date_dir, theader, mask

        comp_inventory_l1, fcb, wave, pol

        good = where(pol eq stokes[ist] and wave eq waves[iw], count)
        if (count eq 0) then begin
          ; this probably shouldn't be a warning now that we have raw
          ; data with various numbers of wavelengths and might be missing
          ; wavelengths in any given file
          ;mg_log, 'no correct pol and wave: %s and %0.2f', $
          ;        stokes[ist], waves[iw], $
          ;        name='comp', /warn
          fits_close, fcb
          continue
        endif
        fits_read, fcb, dat, header, exten_no=good[0] + 1

        naverage[ist, iw] += sxpar(header, 'NAVERAGE')
        data[*, *, ifile] = dat * mask
        if (num_averaged[ist, iw] eq 0) then average_times[0, ist, iw] = strmid(name, 9, 6)
        num_averaged[ist, iw] += 1
        average_times[1, ist, iw] = strmid(name, 9, 6)

        ; sum background images first time through
        if (keyword_set(average_background_by_polarization)) then begin
          background_filename = comp_find_l1_file(date_dir, wave_type, $
                                                  datetime=name, $
                                                  /background)
          fits_open, background_filename, bkg_fcb
          fits_read, bkg_fcb, dat, bkg_header, $
                     extname=string(stokes[ist], waves[iw], format='(%"BKG%s, %0.2f")')
          back[*, *, ist, iw] += dat * mask
          back_naverage[ist, iw] += sxpar(bkg_header, 'NAVERAGE')
          num_back_averaged[ist, iw] += 1
          fits_close, bkg_fcb
        endif else if (ist eq 0) then begin
          background_filename = comp_find_l1_file(date_dir, wave_type, $
                                                  datetime=name, $
                                                  /background)
          fits_open, background_filename, bkg_fcb
          fits_read, bkg_fcb, dat, bkg_header, $
                     extname=string(stokes[ist], waves[iw], format='(%"BKG%s, %0.2f")')
          back[*, *, iw] += dat * mask
          back_naverage[iw] += sxpar(bkg_header, 'NAVERAGE')
          num_back_averaged[iw] += 1
          fits_close, bkg_fcb
        endif

        fits_close, fcb
      endfor

      sxaddpar, header, 'LEVEL   ', 'L2'
      ename = stokes[ist] + ', ' + string(format='(f7.2)', waves[iw])

      ; calculate noise sigma

      ; format times
      hrs  = strmid(average_times[*, ist, iw], 0, 2)
      mins = strmid(average_times[*, ist, iw], 2, 2)
      secs = strmid(average_times[*, ist, iw], 4, 2)
      average_times[*, ist, iw] = hrs + ':' + mins + ':' + secs

      m = numof_stokes[ist]
      sm = sqrt(m)

      sxaddpar, header, 'POLSTATE', stokes[ist]
      sxaddpar, header, 'WAVELENG', waves[iw]
      sxaddpar, header, 'NAVERAGE', m
      sxaddpar, header, 'NFILES', num_averaged[ist, iw], ' Number of files used', $
                after='NAVERAGE'

      start_time = num_averaged[ist, iw] eq 0L ? 0L : average_times[0, ist, iw]
      end_time = num_averaged[ist, iw] eq 0L ? 0L : average_times[1, ist, iw]

      sxaddpar, header, 'AVESTART', start_time, $
                ' [UTC] Start of averaging HH:MM:SS', after='NAVERAGE'
      sxaddpar, header, 'AVEEND', end_time, $
                ' [UTC] End of averaging HH:MM:SS', after='AVESTART'

      sigma = fltarr(nx, nx)
      for ia = 0L, nx - 1L do begin
        for ib = 0L, nx - 1L do begin
          sumx = total(data[ia, ib, *])
          sumx2 = total(data[ia, ib, *]^2)
          xbar = sumx / m
          var = (sumx2 - 2.0 * xbar * sumx + m * xbar^2) / (m - 1.0)
          ; TODO
          ;sigma[ia, ib] = sqrt(var) / sm
          sigma[ia, ib] = sqrt(var)
        endfor
      endfor

      ; write sigma parameters to output files
      fits_write, fcbsig, sigma, header, extname=ename

      ; find median and mean across image
      sxaddpar, header, 'NAVERAGE', naverage[ist, iw]

      med = median(data, dimension=3)
      aver = mean(data, dimension=3)

      ; write Stokes parameters to output files
      if (compute_median) then begin
        sxaddpar, header, 'DATAMIN', min(med), ' MINIMUM DATA VALUE'
        sxaddpar, header, 'DATAMAX', max(med), ' MAXIMUM DATA VALUE'

        nans = where(finite(med, /nan), count)
        if (count gt 0) then begin
          mg_log, 'found NaNs in median result', name='comp', /warn
        endif

        fits_write, fcbmed, med, header, extname=ename
      endif

      if (compute_mean) then begin
        sxaddpar, header, 'DATAMIN', min(aver), ' MINIMUM DATA VALUE'
        sxaddpar, header, 'DATAMAX', max(aver), ' MAXIMUM DATA VALUE'

        nans = where(finite(aver, /nan), count)
        if (count gt 0) then begin
          mg_log, 'found NaNs in mean result', name='comp', /warn
        endif

        fits_write, fcbavg, aver, header, extname=ename
      endif
    endfor
  endfor

  ; write mean background image to output files
  if (keyword_set(average_background_by_polarization)) then begin
    for ist = 0L, n_stokes - 1L do begin
      if (numof_stokes[ist] eq 0) then continue

      for iw = 0L, n_waves - 1L do begin
        back[*, *, ist, iw] /= float(num_back_averaged[ist, iw])
        sxaddpar, header, 'WAVELENG', waves[iw], ' [NM] WAVELENGTH OF OBS'
        sxaddpar, header, 'NAVERAGE', back_naverage[ist, iw]
        sxaddpar, header, 'NFILES', num_back_averaged[ist, iw], ' Number of files used', $
                  after='NAVERAGE'
        sxaddpar, header, 'DATAMIN', min(back[*, *, ist, iw]), ' MINIMUM DATA VALUE'
        sxaddpar, header, 'DATAMAX', max(back[*, *, ist, iw]), ' MAXIMUM DATA VALUE'
        sxaddpar, header, 'POLSTATE', string(stokes[ist], format='(%"BKG%s")')
        ename = string(stokes[ist], waves[iw], format='(%"B%s, %7.2f")')
        if (compute_median) then begin
          nans = where(finite(back[*, *, ist, iw], /nan), count)
          if (count gt 0) then begin
            mg_log, 'found %d NaNs in median values of background', count, $
                    name='comp', /warn
          endif
          fits_write, fcbmed, back[*, *, ist, iw], header, extname=ename
        endif
        if (compute_mean) then begin
          nans = where(finite(back[*, *, ist, iw], /nan), count)
          if (count gt 0) then begin
            mg_log, 'found %d NaNs in mean values of background', count, $
                    name='comp', /warn
          endif

          fits_write, fcbavg, back[*, *, ist, iw], header, extname=ename
        endif
      endfor
    endfor
  endif else begin
    for iw = 0L, n_waves - 1L do begin
      back[*, *, iw] /= float(num_back_averaged[iw])
      sxaddpar, header, 'WAVELENG', waves[iw], ' [NM] WAVELENGTH OF OBS'
      sxaddpar, header, 'NAVERAGE', back_naverage[iw]
      sxaddpar, header, 'NFILES', num_back_averaged[iw], ' Number of files used', $
                after='NAVERAGE'
      sxaddpar, header, 'DATAMIN', min(back[*, *, iw]), ' MINIMUM DATA VALUE'
      sxaddpar, header, 'DATAMAX', max(back[*, *, iw]), ' MAXIMUM DATA VALUE'
      sxaddpar, header, 'POLSTATE', 'BKG'
      ename = 'B, ' + string(format='(f7.2)', waves[iw])
      if (compute_median) then begin
        nans = where(finite(back[*, *, iw], /nan), count)
        if (count gt 0) then begin
          mg_log, 'found NaNs in median values of background', name='comp', /warn
        endif
        fits_write, fcbmed, back[*, *, iw], header, extname=ename
      endif
      if (compute_mean) then begin
        nans = where(finite(back[*, *, iw], /nan), count)
        if (count gt 0) then begin
          mg_log, 'found NaNs in mean values of background', name='comp', /warn
        endif

        fits_write, fcbavg, back[*, *, iw], header, extname=ename
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

  mg_log, 'done', name='comp', /info
end
