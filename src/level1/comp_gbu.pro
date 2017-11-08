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
;     4   background > 30 ppm
;     8   background anamolously low, defined as < 4 ppm  
;     16  standard deviation of intensity image - median intensity
;         image > 2.5 ppm
;     32  background changes abruptly by more than 40% of the median background
;         level
;     64  background image contains more than 150 pixels with a value > 150
;    128  standard deviation of intensity image - median intensity image = NAN
;         or Inf
;
; Output files::
;
;   YYYYMMDD.comp.good.synoptic.WWWW.files.txt
;     - file containing the filenames and metadata for the good synoptic data
;       files for that day
;   YYYYMMDD.comp.good.iqu.WWWW.files.txt
;     - file containing the filenames and metadata for the all good
;       polarization data files for that day (before 9.1.2016 only good 5-pt
;       files, now all good files with Q and U)
;   YYYYMMDD.comp.good.all.WWWW.files.txt
;     - file containing the filenames and metadata for all good data files that
;       day (polarization and waves)
;   YYYYMMDD.comp.good.waves.WWWW.files.txt
;     - file containing the filenames and metadata for all good waves data
;       files that day
;   YYYYMMDD.comp.GBU.WWWW.log
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
;   comp_configuration, comp_get_time_from_filename, comp_make_mask, fits_open,
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

  ; configure
  comp_initialize, date_dir
  comp_configuration

  mg_log, 'wave_type %s', wave_type, name='comp', /info

  catch, error
  if (error ne 0) then begin
    catch, /cancel
    mg_log, /last_error, name='comp'
    return
  endif

  process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)
  if (~file_test(process_dir, /directory)) then begin
    mg_log, 'level1 process directory %s does not exist, exiting', process_dir, $
            name='comp', /warn
    return
  endif
  cd, process_dir

  all_files = wave_type + '_files.txt'    ; get number of filenames in file
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
  back = fltarr(n_files)
  time = fltarr(n_files)
  sigma = fltarr(n_files)
  n_waves = intarr(n_files)
  polstates = strarr(n_files)

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
      mg_log, 'L1 file for %s doesn''t exist on disk but is in inventory file', $
              datetime, $
              name='comp', /warn
      if (perform_gbu) then good_files[ifile] += 1
      continue
    endif

    back_filter = datetime + '.comp.' + wave_type + '.[iquv]*.[1-9]{,[1-9]}.bkg.fts.gz'
    back_name = (file_search(search_filter, count=n_name_found))[0]

    if (n_name_found lt 1L || ~file_test(name)) then begin
      mg_log, 'L1 background %s doesn''t exist on disk but is in inventory file', $
              back_filter, $
              name='comp', /warn
      continue
    endif

    fits_open, name, fcb     ; open fits file
    num_ext = fcb.nextend    ; get number of fits extensions

    fits_open, back_name, back_fcb

    ; read primary header
    fits_read, fcb, d, header, /header_only, exten_no=0

    file_background = sxpar(header, 'BACKGRND')
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
        mg_log, 'standard 3 wavelengths not found, skipping observation %s', str, $
                name='comp', /warn
        if (perform_gbu) then good_files[ifile] += 2

        ; skip observation
        fits_close, fcb
        fits_close, back_fcb
        continue
      endif

      ; reject high background images
      if (back[ifile] gt gbu_max_background) then begin
        mg_log, 'background gt %0.1f, reject %s', gbu_max_background, str, $
                name='comp', /warn
        if (perform_gbu) then good_files[ifile] += 4
      endif

      ; reject anamolously low background images
      if (back[ifile] lt gbu_min_background) then begin
        mg_log, 'background lt %0.1f, reject %s', gbu_min_background, str, $
                name='comp', /warn
        if (perform_gbu) then good_files[ifile] += 8
      endif
    endif

    ; make mask of field-of-view
    comp_make_mask, date_dir, header, mask

    ; find standard 3 wavelengths for the wave type
    ; read line center intensity
    fits_read, fcb, dat, header, exten_no=wave_indices[1] + 1
    ; read blue wing intensity
    fits_read, fcb, dat_b, header, exten_no=wave_indices[0] + 1
    ; read red wing intensity
    fits_read, fcb, dat_r, header, exten_no=wave_indices[2] + 1

    ; use average of intensities (/2 not 3 since blue and red are about 0.5
    ; center intensity)
    data[*, *, ifile] = (dat + dat_b + dat_r) * mask / 2.

    ; read central background image
    fits_read, back_fcb, dat_back, header, exten_no=wave_indices[1] + 1

    ; reject file if there are more than 150 background pixels with a level of
    ; >150
    good = where(mask eq 1)
    g150 = where(dat_back[good] gt 150, g150_count)

    if (wave_type ne '1083') then begin
      if (g150_count gt 150) then begin
        if (perform_gbu) then good_files[ifile] += 64
      endif
    endif

    fits_close, fcb
    fits_close, back_fcb
  endfor

  ; check for dirty O1 using the background level in the morning, as long as
  ; there are observations then skip the first one of the day
  morning = where(time lt 21, count)
  if (count gt 1) then begin
    med_back = median(back[morning[1:*]])
  endif else begin
    ; skip first image of the day, if there are more than 1 images
    if (n_files gt 1L) then begin
      med_back = median(back[1:*])
    endif else begin
      med_back = back[0]
    endelse
  endelse
  mg_log, 'median background %f', med_back, name='comp', /info
  if (med_back gt gbu_med_background) then begin
    mg_log, 'background median exceeds %0.1f, 01 might need to be cleaned', $
            gbu_med_background, $
            name='comp', /warn
  endif

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

  ; create mask of good pixels
  good = where(med gt 1.0, count)
  if (count eq 0) then mg_log, 'med le 1', name='comp', /warn

  ; take difference between data and median image to get pseudo sigma
  for ifile = 0L, n_files - 1L do begin
    diff = abs(data[*, *, ifile] - med)
    sigma[ifile] = mean(diff[good])
  endfor

  sigma /= median(sigma)

  ; test for large sigma defind as > 2.5
  if (wave_type ne '1083') then begin
    bad = where(sigma gt gbu_max_sigma, count)
    if (count gt 0) then if (perform_gbu) then good_files[bad] += 16
  endif

  ; test where background changes radically (defined as 40% of background)
  if (wave_type ne '1083') then begin
    run_back = run_med(back, 10)
    bad = where(abs(back - run_back) gt 0.4 * med_back, count)
    if (count gt 0) then if (perform_gbu) then good_files[bad] += 32
  endif

  ; test for sigma = NaN
  if (wave_type ne '1083') then begin
    bad = where(finite(sigma) eq 0, count)
    if (count gt 0) then if (perform_gbu) then good_files[bad] += 128
  endif

  ; make new text file for synoptic program (first files with n_waves = 5)
  openw, synoptic_lun, $
         string(date_dir, wave_type, format='(%"%s.comp.good.synoptic.%s.files.txt")'), $
         /get_lun

  ; start with synoptic flag on, turn off when hit waves cookbook
  synoptic_flag = 1

  ; make new text file for good waves files
  openw, good_waves_lun, $
         string(date_dir, wave_type, format='(%"%s.comp.good.waves.%s.files.txt")'), $
         /get_lun

  ; make new text file with only good filenames
  openw, good_lun, $
         string(date_dir, wave_type, format='(%"%s.comp.good.iqu.%s.files.txt")'), $
         /get_lun

  ; GBU log file
  openw, gbu_lun, 'GBU.' + wave_type + '.log', /get_lun 

  ; make new file for all good files (both 5 wave and 3 wave)
  openw, good_all_lun, $
         string(date_dir, wave_type, format='(%"%s.comp.good.all.%s.files.txt")'), $
         /get_lun

  printf, gbu_lun, 'Filename                                   Quality     Back     Sigma   #waves  Reason'

  for i = 0L, n_files - 1L do begin
    ; don't put nonexistent files in the GBU file
    if (filenames[i] eq '') then continue

    ; stop saving to synoptic when obs switch to 3 waves
    if ((synoptic_flag eq 1) and (n_waves[i] lt 5)) then synoptic_flag = 0
    if ((synoptic_flag eq 1) and (good_files[i] eq 0)) then begin
      printf, synoptic_lun, good_lines[i]
    endif
    if ((good_files[i] eq 0) $
          and (strpos(polstates[i], 'Q') ge 0) $
          and (strpos(polstates[i], 'U') ge 0)) then begin
      printf, good_lun, good_lines[i]
    endif
    ; need fast cadence for waves (only way to tell now is 3 points)
    if ((good_files[i] eq 0) and (n_waves[i] eq 3)) then begin
      printf, good_waves_lun, good_lines[i]
    endif
    if (good_files[i] eq 0) then printf, good_all_lun, good_lines[i]
    printf, gbu_lun, $
            filenames[i], $
            good_files[i] eq 0 ? '  Good' : '   Bad', $
            back[i], $
            sigma[i], $
            n_waves[i], $
            good_files[i], $
            format='(A41, X, A6, X, F10.2, F10.2, 2x, I5, 3x, i3)'
  endfor

  free_lun, lun
  free_lun, synoptic_lun
  free_lun, good_lun
  free_lun, gbu_lun
  free_lun, good_waves_lun
  free_lun, good_all_lun

  mg_log, '%d total files', n_files, name='comp', /info
  bad = where(good_files gt 0, bad_total)
  mg_log, '%d bad files', bad_total, name='comp', /info
  if (bad_total gt 0) then begin
    bad_existing = where(good_files gt 0 and good_files mod 2 eq 0, n_bad_existing)
    if (n_bad_existing gt 0L) then begin
      mg_log, 'bad: %s', strjoin(filenames[bad_existing], ', '), name='comp', /warn
    endif
  endif

  ; engineering plots
  eng_dir = filepath('', subdir=comp_decompose_date(date_dir), root=engineering_dir)
  if (~file_test(eng_dir, /directory)) then file_mkdir, eng_dir

  write_csv, filepath(date_dir + '.comp.' + wave_type + '.qa_sigma.txt', $
                      root=eng_dir), $
             time, sigma

  write_csv, filepath(date_dir + '.comp.' + wave_type + '.qa_background.txt', $
                      root=eng_dir), $
             time, back

  mg_log, 'done', name='comp', /info
end


; main-level program to run COMP_GBU outside of the pipeline

date_dir = '20150624'
mg_log, name='comp', logger=logger
logger->setProperty, level=2

; initialize comp_constants_common
comp_initialize, date_dir

; initialize comp_config_common
config_filename = filepath('comp.mgalloy.pike.cfg', $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())
comp_configuration, config_filename=config_filename
 
comp_gbu, date_dir, '1074', error=error
help, error

end
