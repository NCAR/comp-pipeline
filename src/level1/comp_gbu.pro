; docformat = 'rst'

;+
; Procedure to read CoMP compressed Level_1 data files containing demodulated
; Stokes parameters and determine if the data in that file are considered
; 'good'.
;
; Since not all Stokes parameters are observed for all Level_1 data, this
; procedure uses only Stokes I and background data.
;
; Several metrics are used to test the data. A parameter, good_files, is
; computed which is 0 if the data is good, and incremented by a bit value for
; each of the tests that fail. Since there are 8 metrics for rejection, the
; `good_files` paramater can have a value between 0 and 255. The metrics for
; rejection and the corresponding bits set in the good_files parameter are::
;
;     1   data doesn't exist on disk but is in inventory file
;     2   3 standard wavelengths not found (not necessarily bad data)
;     4   background > 16.0 ppm
;     8   background anamolously low, defined as < 1 ppm  
;     16  standard deviation of intensity image - median intensity
;         image > 2.5 ppm
;     32  background changes abruptly by more than 50% of the median background
;         level
;     64  background image contains more than 2000 pixels with a value > 70.0
;    128  standard deviation of intensity image - median intensity image = NaN
;         or Inf
;    256  intensity image contains more than 60% pixels with a value < 0.1
;
; Output files::
;
;   YYYYMMDD.comp.WWWW.good.synoptic.files.txt
;     - file containing the filenames and metadata for the good synoptic data
;       files for that day
;   YYYYMMDD.comp.WWWW.good.iqu.files.txt
;     - file containing the filenames and metadata for the all good
;       polarization data files for that day (before 9.1.2016 only good 5-pt
;       files, now all good files with Q and U)
;   YYYYMMDD.comp.WWWW.good.all.files.txt
;     - file containing the filenames and metadata for all good data files that
;       day (polarization and waves)
;   YYYYMMDD.comp.WWWW.good.waves.files.txt
;     - file containing the filenames and metadata for all good waves data
;       files that day
;   YYYYMMDD.comp.WWWW.gbu.log
;     - file containing the filenames, the the background, the sigma parameter
;       and the good_files parameter
;
; where `WWWWW` is the wavelength range ('1074', '1079' or '1083') and
; `YYYYMMDD` is the date of the data acquisition. The 'synoptic' data are
; defined as the polarization data taken before the first waves sequence.
;
;   postscript plot of the background
;   postscript plot of the sigma parameter
;
; The first 5 files are written in the processing directory and the plots are
; written in the Engineering directory.
;
; Note: This routine was modified to read uncompressed FITS files
;
; :Examples:
;   For example, call like this::
;
;     comp_gbu, '20130531', '1074'
;
; :Uses:
;   comp_constants_common, comp_config_common, comp_initialize,
;   comp_configuration, comp_get_time_from_filename, comp_l1_mask, fits_open,
;   fits_read, fits_close, sxpar
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
;   MLSO Software Team
;
; :History:
;   Removed gzip Oct 1 2014 GdT
;   Removed file_copy to engineering directory  Oct 1 2014 GdT
;   see git log for recent changes
;-
pro comp_gbu, date_dir, wave_type, error=error
  compile_opt idl2
  @comp_constants_common
  @comp_config_common

  mg_log, 'wave type %s nm', wave_type, name='comp', /info

  catch, error
  if (error ne 0) then begin
    catch, /cancel
    mg_log, /last_error, name='comp'
    return
  endif

  eng_dir = filepath('', subdir=comp_decompose_date(date_dir), root=engineering_dir)
  if (~file_test(eng_dir, /directory)) then file_mkdir, eng_dir

  process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)
  if (~file_test(process_dir, /directory)) then begin
    mg_log, 'level1 process directory %s does not exist, exiting', process_dir, $
            name='comp', /warn
    return
  endif
  cd, process_dir

  ; get number of filenames in file
  all_files = string(date_dir, wave_type, format='(%"%s.comp.%s.files.txt")')
  n_files = file_lines(all_files)         ; get number of filenames in file

  if (n_files lt 1L) then begin
    mg_log, 'no files for wave type %s', wave_type, /warning, name='comp'
    return
  endif

  filenames = strarr(n_files)

  ; array to collect good filenames (0 for good, >0 for bad)
  good_files = intarr(n_files)
  good_lines = strarr(n_files)

  data = fltarr(nx, nx, n_files, /nozero)   ; array for images

  ; arrays for header data
  offset = bytarr(n_files)
  back = fltarr(n_files) + !values.f_nan
  time = fltarr(n_files)
  img_sigma = fltarr(n_files)
  lt_threshold_count = fltarr(n_files)
  gt_threshold_count = fltarr(n_files)
  n_waves = intarr(n_files)
  polstates = strarr(n_files)

  background_thresholds = [60.0, 70.0, 100.0]
  background_counts = lonarr(n_elements(background_thresholds), n_files)

  str = ''
  openr, lun, all_files, /get_lun

  for ifile = 0L, n_files - 1L do begin
    readf, lun, str
    good_lines[ifile] = str

    datetime = strmid(str, 0, 15)

    ; search_filter is a glob, not a regular expression
    search_filter = datetime + '.comp.' + wave_type + '.[iquv]*.[1-9]{,[1-9]}.fts.gz'
    name = (file_search(search_filter, count=n_name_found))[0]

    filenames[ifile] = name
    time[ifile] = comp_get_time_from_filename(name)

    if (n_name_found lt 1L) then begin
      mg_log, 'no L1 file for %s', $
              datetime, $
              name='comp', /warn
      if (perform_gbu) then good_files[ifile] += 1
      continue
    endif

    back_filter = datetime + '.comp.' + wave_type + '.[iquv]*.[1-9]{,[1-9]}.bkg.fts.gz'
    back_name = (file_search(back_filter, count=n_name_found))[0]

    if (wave_type ne '1083' && (n_name_found lt 1L || ~file_test(name))) then begin
      mg_log, 'no L1 background %s', $
              back_filter, $
              name='comp', /warn
      continue
    endif

    fits_open, name, fcb     ; open fits file
    num_ext = fcb.nextend    ; get number of fits extensions

    if (wave_type ne '1083') then begin
      fits_open, back_name, back_fcb
    endif

    ; read primary header
    fits_read, fcb, d, header, /header_only, exten_no=0, /no_abort, message=msg
    if (msg ne '') then message, msg

    offset[ifile] = strtrim(sxpar(header, 'OCC-PNTG'), 2) eq 'OFFSET'
    file_background = sxpar(header, 'LCBKG')
    back[ifile] = size(file_background, /type) eq 7 ? !values.f_nan : file_background
    n_waves[ifile] = sxpar(header, 'NTUNES')
  
    comp_inventory, fcb, beam, wavelengths, pol, error=error
    if (error gt 0L) then begin
      mg_log, 'skipping %s', name, name='comp', /error
      continue
    endif

    upol = pol[uniq(pol, sort(pol))]
    polstates[ifile] = strjoin(upol, ',')

    wave_indices = comp_3pt_indices(wave_type, wavelengths, error=wave_error)  

    ; reject special obs at beginning with number of wavelengths observed > 10
    if (wave_type ne '1083') then begin
      ; skip observation if standard 3 wavelengths don't exist for the wave type
      if (wave_error gt 0L) then begin
        mg_log, '%s: standard 3 wavelengths not found', name, $
                name='comp', /warn
        if (perform_gbu) then good_files[ifile] += 2

        ; skip observation
        fits_close, fcb
        fits_close, back_fcb
        continue
      endif

      ; reject high background images
      if (back[ifile] gt gbu_max_background) then begin
        mg_log, '%s: bkg %0.1f > max %0.1f', $
                name, back[ifile], gbu_max_background, $
                name='comp', /warn
        if (perform_gbu) then good_files[ifile] += 4
      endif

      ; reject anomalously low background images
      if (back[ifile] lt gbu_min_background) then begin
        mg_log, '%s: bkg %0.1f < min %0.1f', $
                name, back[ifile], gbu_min_background, $
                name='comp', /warn
        if (perform_gbu) then good_files[ifile] += 8
      endif
    endif

    ; make mask of field-of-view
    mask = comp_l1_mask(header)

    ; find standard 3 wavelengths for the wave type
    ; read line center intensity
    fits_read, fcb, dat, header, exten_no=wave_indices[1] + 1, /no_abort, message=msg
    if (msg ne '') then message, msg
    ; read blue wing intensity
    fits_read, fcb, dat_b, header, exten_no=wave_indices[0] + 1, /no_abort, message=msg
    if (msg ne '') then message, msg
    ; read red wing intensity
    fits_read, fcb, dat_r, header, exten_no=wave_indices[2] + 1, /no_abort, message=msg
    if (msg ne '') then message, msg

    ; use average of intensities (/2 not 3 since blue and red are about 0.5
    ; center intensity)
    data[*, *, ifile] = (dat + dat_b + dat_r) * mask / 2.0

    if (wave_type ne '1083') then begin
      ; read central background image
      fits_read, back_fcb, dat_back, header, exten_no=wave_indices[1] + 1, $
                 /no_abort, message=msg
      if (msg ne '') then message, msg

      annulus_indices = where(mask, n_annulus_indices)
      if (n_annulus_indices gt 0L) then begin
        ifile_intensity = reform(data[*, *, ifile])
        !null = where(ifile_intensity[annulus_indices] lt gbu_intensity_min_threshold, n_below)
        lt_threshold_count[ifile] = n_below
        if (n_below gt n_annulus_indices * gbu_intensity_percentage / 100.0) then begin
          mg_log, '%s: %d int pixels < %0.3f', $
                  name, n_below, gbu_intensity_min_threshold, $
                  name='comp', /warn
          good_files[ifile] += 256
        endif
      endif

      ; reject file if there are more than 100 background pixels with a level of
      ; > 70.0
      ;good = where(mask eq 1)   ; not using right now

      gt_threshold = where(dat_back gt gbu_background_threshold, $
                           file_gt_threshold_count)

      gt_threshold_count[ifile] = file_gt_threshold_count
      if (file_gt_threshold_count ge gbu_threshold_count) then begin
        mg_log, '%s: %d bkg pixels > %0.1f', $
                name, file_gt_threshold_count, gbu_background_threshold, $
                name='comp', /warn
        if (perform_gbu) then good_files[ifile] += 64
      endif

      for b = 0L, n_elements(background_thresholds) - 1L do begin
        gt_threshold = where(dat_back gt background_thresholds[b], $
                             threshold_count)
        background_counts[b, ifile] = threshold_count
      endfor
    endif

    fits_close, fcb
    if (wave_type ne '1083') then fits_close, back_fcb
  endfor

  ; check for dirty O1 using the background level in the morning, as long as
  ; there are observations then skip the first one of the day
  if (wave_type ne '1083') then begin
    morning = where(time gt 16 and time lt 21, count)
    if (count gt 1) then begin
      med_back = median(back[morning[1:*]])

      if (med_back gt gbu_med_background) then begin
        mg_log, 'bkg median %0.1f exceeds %0.1f, 01 might need to be cleaned', $
                med_back, gbu_med_background, $
                name='comp', /warn
      endif
    endif else begin
      ; skip first image of the day, if there are more than 1 images
      if (n_files gt 1L) then begin
        med_back = median(back[1:*])
        
        if (med_back gt gbu_med_background) then begin
          mg_log, 'after 2100 UT bkg median %0.1f exceeds %0.1f', $
                  med_back, gbu_med_background, $
                  name='comp', /warn
        endif
      endif else begin
        med_back = back[0]
      endelse
    endelse

    mg_log, 'median background %0.1f', med_back, name='comp', /info
  endif else begin
    med_back = !values.f_nan
  endelse

  ; find median intensity image using only good images so far
  good_subs = where(good_files eq 0, count)
  if (count eq 0) then begin
    mg_log, 'no good %s files this day', wave_type, name='comp', /warn
  endif

  med = fltarr(nx, nx)
  if (count gt 0) then begin
    for ia = 0L, nx - 1L do begin
      for ib = 0L, nx - 1L do begin
        med[ia, ib] = median(data[ia, ib, good_subs])
      endfor
    endfor
  endif

  ; save med in .sav file
  med_basename = string(date_dir, wave_type, format='(%"%s.comp.%s.median.gbu.sav")')
  med_filename = filepath(med_basename, root=eng_dir)
  save, med, filename=med_filename

  ; check if there are good images
  good = where(med gt 1.0, count)
  if (count eq 0) then begin
    mg_log, 'median <= 1', name='comp', /warn
  endif else begin
    for ifile = 0L, n_files - 1L do begin
      ; take difference between data and median image to get pseudo sigma
      diff = data[*, *, ifile] - med

      ; neglect pixels that are masked, i.e. zero pixels
      ; neglect pixel that are below 1.0
      good = where(data[*, *, ifile] gt 1 and med gt 1, n_good)
      if (n_good gt 0L) then begin
        img_sigma[ifile] = stddev(diff[good])
      endif else begin
        mg_log, 'not computing sigma for %s [%d]', filenames[ifile], ifile, name='comp', /warn
        img_sigma[ifile] = !values.f_nan
      endelse
    endfor

    ; test for large sigma defind as > 2.5
    if (wave_type ne '1083') then begin
      bad = where(img_sigma gt gbu_max_sigma, count)
      if (count gt 0L) then begin
        mg_log, 'sigma > max sigma %0.2f at %d %s nm files', $
                gbu_max_sigma, count, wave_type, $
                name='comp', /warn
        if (perform_gbu) then good_files[bad] += 16
      endif
    endif
  endelse

  ; test where background changes radically (defined as 50% of background)
  if (wave_type ne '1083') then begin
    back_good = where(good_files eq 0, n_back_good)

    ; if all files are bad, we don't tack on this reason as well
    if (n_back_good gt 0L) then begin
      run_back = run_med(back[back_good], 10)
      change_threshold = gbu_percent_background_change * back[back_good]
      bad = where(abs(back - run_back) gt change_threshold, count)
      if (count gt 0) then if (perform_gbu) then good_files[bad] += 32
    endif
  endif

  ; test for sigma = NaN
  if (wave_type ne '1083') then begin
    bad = where(finite(img_sigma) eq 0, count)
    if (count gt 0) then if (perform_gbu) then good_files[bad] += 128
  endif

  ; make new text file for synoptic program (first files with n_waves = 5)
  openw, good_synoptic_lun, $
         string(date_dir, wave_type, format='(%"%s.comp.%s.good.synoptic.files.txt")'), $
         /get_lun

  ; start with synoptic flag on, turn off when hit waves cookbook
  synoptic_flag = 1

  ; make new text file for good waves files
  openw, good_waves_lun, $
         string(date_dir, wave_type, format='(%"%s.comp.%s.good.waves.files.txt")'), $
         /get_lun

  ; make new text file with only good filenames
  openw, good_iqu_lun, $
         string(date_dir, wave_type, format='(%"%s.comp.%s.good.iqu.files.txt")'), $
         /get_lun

  ; GBU log file
  openw, gbu_lun, $
         string(date_dir, wave_type, format='(%"%s.comp.%s.gbu.log")'), $
         /get_lun 

  ; make new file for all good files (both 5 wave and 3 wave)
  openw, good_all_lun, $
         string(date_dir, wave_type, format='(%"%s.comp.%s.good.all.files.txt")'), $
         /get_lun

  printf, gbu_lun, med_back, median([back]), $
          format='(%"Median morning background: %0.2f, median background: %0.2f")'
  printf, gbu_lun, $
          'Filename', 'Quality', 'Back', 'Sigma', $
          string(gbu_intensity_min_threshold, format='(%"#<%4.1f")'), $
          string(gbu_background_threshold, format='(%"#>%4.1f")'), $
          '#waves', 'Reason', $
          format='(A-41, X, A7, X, A6, X, A6, 2X, A6, 2X, A6, 2X, A6, 2X, A6)'

  for i = 0L, n_files - 1L do begin
    ; don't put nonexistent files in the GBU file
    if (filenames[i] eq '') then continue

    ; stop saving to synoptic when obs switch to 3 waves
    if ((synoptic_flag eq 1) and (n_waves[i] lt 5)) then synoptic_flag = 0
    if ((synoptic_flag eq 1) and (good_files[i] eq 0) $
          and (gt_threshold_count[i] lt gbu_offset_count)) then begin
      printf, good_synoptic_lun, good_lines[i]
    endif
    if ((good_files[i] eq 0) $
          and (strpos(polstates[i], 'Q') ge 0) $
          and (strpos(polstates[i], 'U') ge 0) $
          and (gt_threshold_count[i] lt gbu_offset_count)) then begin
      printf, good_iqu_lun, good_lines[i]
    endif
    ; need fast cadence for waves (only way to tell now is 3 points)
    if ((good_files[i] eq 0) and (n_waves[i] eq 3) $
        and (gt_threshold_count[i] lt gbu_offset_count)) then begin
      printf, good_waves_lun, good_lines[i]
    endif
    if ((good_files[i] eq 0) $
          and (gt_threshold_count[i] lt gbu_offset_count)) then begin
      printf, good_all_lun, good_lines[i]
    endif

    printf, gbu_lun, $
            filenames[i], $
            good_files[i] ne 0 $
              ? 'Bad' $
              : (((gt_threshold_count[i] ge gbu_offset_count) || offset[i]) ? 'Offset' : 'Good'), $
            back[i], $
            img_sigma[i], $
            lt_threshold_count[i], $
            gt_threshold_count[i], $
            n_waves[i], $
            good_files[i], $
            format='(A41, X, A7, X, F6.2, X, F6.2, 2X, I6, 2X, I6, 2X, I6, 2X, i6)'
  endfor

  free_lun, lun
  free_lun, good_synoptic_lun
  free_lun, good_iqu_lun
  free_lun, gbu_lun
  free_lun, good_waves_lun
  free_lun, good_all_lun

  bad = where(good_files gt 0, bad_total)
  mg_log, '%d bad files out of %d total %s files', $
          bad_total, n_files, wave_type, name='comp', /info

  bkg_counts_basename = string(date_dir, wave_type, $
                               format='(%"%s.comp.%s.gbu.bkg.counts.txt")')
  openw, lun, filepath(bkg_counts_basename, root=eng_dir), /get_lun
  for f = 0L, n_files - 1L do begin
    printf, lun, filenames[f], $
            background_counts[0, f], $
            background_counts[1, f], $
            background_counts[2, f], $
            format='(%"%-45s%10d%10d%10d")'
  endfor
  free_lun, lun

  ; engineering plots
  write_csv, filepath(date_dir + '.comp.' + wave_type + '.qa_sigma.txt', $
                      root=eng_dir), $
             time, img_sigma

  write_csv, filepath(date_dir + '.comp.' + wave_type + '.qa_background.txt', $
                      root=eng_dir), $
             time, back

  mg_log, 'done', name='comp', /info
end


; main-level program to run COMP_GBU outside of the pipeline

;dates = ['20171001', '20171105', '20171121', '20171124', '20171201', $
;         '20171202', '20171203', '20171204', '20171205', '20171206', $
;         '20171207', '20171208', '20171209', '20171210', '20171211', $
;         '20171212', '20171213', '20171214', '20171216']
dates = ['20171012']
config_filename = filepath('comp.reprocess-check-2017.cfg', $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())
comp_configuration, config_filename=config_filename
wave_type = '1074'

for d = 0L, n_elements(dates) - 1L do begin
  comp_initialize, dates[d]
  comp_gbu, dates[d], wave_type, error=error
endfor

end
