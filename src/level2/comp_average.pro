; docformat = 'rst'

;+
; Procedure to read CoMP Level_1 data files containing demodulated Stokes
; parameters and compute the mean, median and standard deviation of the like
; images in those files. This routine is able to handle a set of files to
; average which contain different wavelength and Stokes parameter images.
;
; Note: the background images will also be mean images.
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
;     comp_average, '20110316', '1074', list_file = 'files.txt'
;
;   will use filename 'files.txt' as input file. The following::
;
;     comp_average, '20110902', '1074', /synoptic
;
;   will use filename 'synoptic_1074_files.txt' as input filename. Finally::
;
;     comp_average, '20131026', '1074'
;
;   will use 'good_1074_files.txt' as input filename.
;
; :Uses:
;   comp_config_common, comp_constants_common, comp_inventory_l2, comp_uniq,
;   comp_make_mask, comp_find_l1_file, fits_open, fits_read, fits_write,
;   fits_close, sxdelpar, sxaddpar, mg_log
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :Keywords:
;   list_file : in, optional, type=string, default=good_XXXX_files.txt
;     filename containing list of files to process
;   synoptic
;     optional keyword to processes 'synoptic' subset of images
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
pro comp_average, date_dir, wave_type, list_file=list_file, synoptic=synoptic, $
                  error=error
  compile_opt idl2
  @comp_config_common
  @comp_constants_common

  mg_log, 'wave_type: %s', wave_type, name='comp', /info

  ; Establish error handler. When errors occur, the index of the
  ; error is returned in the variable Error_status:
  catch, error
  if (error ne 0) then begin
    catch, /cancel
    mg_log, /last_error, name='comp'
    return
  endif

  ; check keywords
  uselist = 0
  if (n_elements(list_file) eq 1) then uselist = 1

  process_dir = filepath(date_dir, root=process_basedir)
  cd, process_dir

  mean_opt = 'yes'   ; compute mean? (yes or no)
  median_opt = 'yes'   ; compute median? (yes or no)

  ; create output fits file

  if (mean_opt eq 'yes') then begin
    fits_open, date_dir + '.comp.' + wave_type + '.mean.fts', fcbavg, /write
  endif
  if (median_opt eq 'yes') then begin
    fits_open, date_dir + '.comp.' + wave_type + '.median.fts', fcbmed, /write
  endif
  fits_open, date_dir + '.comp.' + wave_type + '.sigma.fts', fcbsig, /write

  if (uselist) then begin
    files = list_file
    n_files = file_lines(list_file)
  endif else begin
    if (keyword_set(synoptic)) then begin
      ; file with list of filenames
      files = 'synoptic_' + wave_type + '_files.txt'
    endif else begin
      ; file with list of filenames
      files = 'good_' + wave_type + '_files.txt'
    endelse
    n_files = file_lines(files)
    ; average, at most, the first 50 files
    ; n_files <= 50        ; commented out 6/16/14 ST
  endelse

  mg_log, 'using %d files from %s', n_files, files, name='comp', /info

  if (n_files lt 1) then return

  openr, lun, files, /get_lun
  str = ' '

  ; loop over filenames and determine number of each images for each Stokes
  ; parameter
  numof_stokes = intarr(n_stokes)
  which_file = intarr(n_files, n_stokes)
  filenames = strarr(n_files)

  for j = 0L, n_files - 1L do begin
    readf, lun, str
    filenames[j] = strmid(str, 0, 15)
    for i = 0L, n_stokes - 1L do begin
      sp = strpos(str, stokes[i])
      if (sp gt -1) then begin
        which_file[numof_stokes[i], i] = j
        ++numof_stokes[i]
      endif
    endfor
  endfor

  free_lun, lun

  ; time period for this averaging
  year     = long(strmid(filenames[0], 0, 4))
  month    = long(strmid(filenames[0], 4, 2))
  day      = long(strmid(filenames[0], 6, 2))
  hour     = long(strmid(filenames[0], 9, 2))
  minute   = long(strmid(filenames[0], 11, 2))
  second   = long(strmid(filenames[0], 13, 2))
  start_jd = julday(month, day, year, hour, minute, second)

  year    = long(strmid(filenames[n_files-1], 0, 4))
  month   = long(strmid(filenames[n_files-1], 4, 2))
  day     = long(strmid(filenames[n_files-1], 6, 2))
  hour    = long(strmid(filenames[n_files-1], 9, 2))
  minute  = long(strmid(filenames[n_files-1], 11, 2))
  second  = long(strmid(filenames[n_files-1], 13, 2))
  end_jd = julday(month, day, year, hour, minute, second)

  duration = end_jd - start_jd
  mid_jd = start_jd + duration / 2
  caldat, mid_jd, utmonth, utday, utyear, uthour, utminute, utsecond

  utyear =   string(utyear, format='(I4)')
  utday =    string(utday, format='(I02)')
  utmonth =  string(utmonth, format='(I02)')
  uthour =   string(uthour, format='(I02)')
  utminute = string(utminute, format='(I02)')
  utsecond = string(round(utsecond), format='(I02)')

  ; construct FITS standard strings
  date_str = utyear + '-' + utmonth + '-' + utday
  time_str = uthour + ':' + utminute + ':' + utsecond
  
  for i = 0L, n_stokes - 1L do begin
    mg_log, '%d image files for Stokes %s', numof_stokes[i], stokes[i], $
            name='comp', /info
  endfor
  ;  for i=0,n_stokes-1 do print,which_file[0:numof_stokes[i]-1,i]

  ; take inventory of first file to find wavelengths
  test_filename = comp_find_l1_file(date_dir, filenames[0], wave_type)
  fits_open, test_filename, fcb

  comp_inventory_l1, fcb, wave, pol

  ; read the primary header to use for the output
  fits_read, fcb, d, primary_header, /header_only, exten_no=0
  sxdelpar, primary_header, 'DATE_HST'
  sxdelpar, primary_header, 'TIME_HST'
  sxaddpar, primary_header, 'LEVEL   ', 'L2'
  fits_close, fcb
  sxaddpar, primary_header, 'DATE-OBS', date_str, $
            ' [UTC] Averaging mid-point DATE: CCYY-MM-DD', after='TIMESYS'
  sxaddpar, primary_header, "TIME-OBS", time_str, $
            ' [UTC] Averaging mid-point TIME: HH:MM:SS', after='DATE-OBS'
  sxaddpar, primary_header, 'DURATION', 24. * 60. * duration, $
            ' [minutes] Averaging duration', after='TIME-OBS', format='(f8.3)'

  sxaddpar, primary_header, 'VERSION', code_revision, ' Software Subversion Revision'

  if (median_opt eq 'yes') then fits_write, fcbmed, 0, primary_header
  if (mean_opt eq 'yes') then fits_write, fcbavg, 0, primary_header

  waves = wave[comp_uniq(wave, sort(wave))]   ; create array of wavelengths
  n_waves = n_elements(waves)

  mg_log, '%s', strjoin(strtrim(waves, 2), ', '), name='comp/average', /debug

  ; compute averages
  back = fltarr(nx, nx, n_waves)
  ; summation of number of images going into average background
  num_back_averaged = intarr(n_waves)

  for ist = 0L, n_stokes - 1L do begin
    if (numof_stokes[ist] eq 0) then continue
    for iw = 0L, n_waves - 1L do begin
      mg_log, 'Stokes %s wave %s', $
              strjoin(strtrim(stokes[ist], 2), ', '), $
              strjoin(strtrim(waves[iw], 2), ', '), $
              name='comp/average', /debug

      data = fltarr(nx, nx, numof_stokes[ist], /nozero)
      num_averaged = 0   ; summation of number of images going into average

      for ifile = 0L, numof_stokes[ist] - 1L do begin
        name = filenames[which_file[ifile, ist]]
        filename = comp_find_l1_file(date_dir, name, wave_type)
        mg_log, 'file %d/%d for %s @ %s: %s', $
                ifile + 1, $
                numof_stokes[ist], $
                strtrim(stokes[ist], 2), $
                strtrim(waves[iw], 2), $
                name, $
                name='comp/average', /debug
        fits_open, filename, fcb
        fits_read, fcb, d, theader, /header_only, exten_no=0
        comp_make_mask, date_dir, theader, mask

        comp_inventory_l1, fcb, wave, pol

        good = where(pol eq stokes[ist] and wave eq waves[iw], count)
        if (count eq 0) then begin
          mg_log, 'no correct pol and wave: %s and %0.2f', $
                  stokes[ist], waves[iw], $
                  name='comp', /warn
          fits_close, fcb
          continue
        endif
        fits_read, fcb, dat, header, exten_no=good[0] + 1

        data[*, *, ifile] = dat * mask
        naverage = sxpar(header, 'NAVERAGE')
        num_averaged += naverage

        ; sum background images first time through
        if (ist eq 0) then begin
          background_filename = comp_find_l1_file(date_dir, name, wave_type, /background)
          fits_open, background_filename, bkg_fcb
          fits_read, bkg_fcb, dat, $
                     extname=string(stokes[ist], waves[iw], format='(%"BKG%s, %0.2f")')
          back[*, *, iw] += dat * mask
          naverage = sxpar(header, 'NAVERAGE')
          num_back_averaged[iw] += naverage
          fits_close, bkg_fcb
        endif

        fits_close,fcb
      endfor

      sxaddpar, header, 'LEVEL   ', 'L2'
      ename = stokes[ist] + ', ' + string(format='(f7.2)', waves[iw])

      ; calculate noise sigma

      m = numof_stokes[ist]
      sm = sqrt(m)
      sxaddpar, header, 'NAVERAGE', m

      sigma = fltarr(nx,nx)
      for ia = 0L, nx - 1L do begin
        for ib = 0L, nx - 1L do begin
          sumx = total(data[ia, ib, *])
          sumx2 = total(data[ia, ib, *]^2)
          xbar = sumx / m
          var = (sumx2 - 2.0 * xbar * sumx + m * xbar^2) / (m - 1.0)
          sigma[ia,ib] = sqrt(var) / sm
        endfor
      endfor

      ; write sigma parameters to output files
      fits_write, fcbsig, sigma, header, extname=ename

      ; find median and mean across image
      sxaddpar, header, 'NAVERAGE', num_averaged

      med = median(data, dimension=3)
      aver = mean(data, dimension=3)

      ; write Stokes parameters to output files
      if (median_opt eq 'yes') then begin
        sxaddpar, header, 'DATAMIN', min(med), ' MINIMUM DATA VALUE'
        sxaddpar, header, 'DATAMAX', max(med), ' MAXIMUM DATA VALUE'

        nans = where(finite(med, /nan), count)
        if (count gt 0) then begin
          mg_log, 'NaNs', name='comp', /warn
        endif

        fits_write, fcbmed, med, header, extname=ename
      endif
      if (mean_opt eq 'yes') then begin
        sxaddpar, header, 'DATAMIN', min(aver), ' MINIMUM DATA VALUE'
        sxaddpar, header, 'DATAMAX', max(aver), ' MAXIMUM DATA VALUE'

        nans = where(finite(aver, /nan), count)
        if (count gt 0) then begin
          mg_log, 'NaNs', name='comp', /warn
        endif

        fits_write, fcbavg, aver, header, extname=ename
      endif
    endfor
  endfor

  ; write mean background image to output files
  for iw = 0L, n_waves - 1L do begin
    back[*,*,iw] /= float(numof_stokes[0])
    sxaddpar, header, 'WAVELENG', waves[iw], ' [NM] WAVELENGTH OF OBS'
    sxaddpar, header, 'NAVERAGE', num_back_averaged[iw]
    sxaddpar, header, 'DATAMIN', min(back[*, *, iw]), ' MINIMUM DATA VALUE'
    sxaddpar, header, 'DATAMAX', max(back[*, *, iw]), ' MAXIMUM DATA VALUE'
    ename = 'B, ' + string(format='(f7.2)', waves[iw])
    if (median_opt eq 'yes') then begin
      nans = where(finite(back[*, *, iw], /nan), count)
      if (count gt 0) then begin
        mg_log, 'NaNs', name='comp', /warn
      endif
      fits_write, fcbmed, back[*, *, iw], header, extname=ename
    endif
    if (mean_opt eq 'yes') then begin
      nans = where(finite(back[*, *, iw], /nan), count)
      if (count gt 0) then begin
        mg_log, 'NaNs', name='comp', /warn
      endif
      fits_write, fcbavg, back[*, *, iw], header, extname=ename
    endif
  endfor

  ; close files
  if (median_opt eq 'yes') then fits_close, fcbmed
  if (mean_opt eq 'yes') then fits_close, fcbavg
  fits_close, fcbsig

  mg_log, 'done', name='comp', /info
end
