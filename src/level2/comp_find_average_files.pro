; docformat = 'rst'

;+
; Look for clusters of `MIN_N_CLUSTER_FILES` files in a row within
; `MAX_CADENCE_INTERVAL` of each other and without a flat between them.
;
; :Returns:
;   `strarr` of filenames or `!null` of no valid cluster found
;
; :Params:
;   list_filename : in, required, type=string
;     filename of candidate files
;   flat_times : in, required, type=fltarr
;     times of the flats in hours from local midnight, not needed if
;     `CALIBRATION` is set
;
; :Keywords:
;   max_cadence_interval : in, required, type=float
;     max time allowed between files before starting a new cluster, in days
;   max_n_files : in, required, type=long
;     maximum number of files to be returned
;   min_n_cluster_files : in, required, type=long
;     minimum number of files needed in a cluster
;   stokes_present : out, optional, type=strarr
;     set to a named variable to retrieve the Stokes variables present in each
;     corresponding file
;   count : out, optional, type=long
;     set to a named variable to retrieve the number of files found
;   calibration : in, optional, type=boolean
;     set to indicate that files suitable for producing mean file for
;     calculting empirical crosstalk coefficients should be returned
;-
function comp_find_average_files_findclusters, list_filename, flat_times, $
                                               date_dir=date_dir, wave_type=wave_type, $
                                               max_cadence_interval=max_cadence_interval, $
                                               min_n_cluster_files=min_n_cluster_files, $
                                               max_n_files=max_n_files, $
                                               stokes_present=stokes_present, $
                                               count=count, $
                                               calibration=calibration
  compile_opt strictarr
  @comp_constants_common

  if (~file_test(list_filename)) then begin
    count = 0L
    return, !null
  endif

  n_candidate_files = file_lines(list_filename)

  if (n_candidate_files eq 0L) then begin
    count = 0L
    return, !null
  endif

  times = dblarr(n_candidate_files)
  stokes_present = strarr(n_candidate_files)

  candidate_files = strarr(n_candidate_files)

  openr, lun, list_filename, /get_lun
  line = ''
  for f = 0L, n_candidate_files - 1L do begin
    readf, lun, line

    tokens = strsplit(line, /extract)
    candidate_files[f] = tokens[0]

    year   = long(strmid(candidate_files[f],  0, 4))
    month  = long(strmid(candidate_files[f],  4, 2))
    day    = long(strmid(candidate_files[f],  6, 2))
    hour   = long(strmid(candidate_files[f],  9, 2))
    minute = long(strmid(candidate_files[f], 11, 2))
    second = long(strmid(candidate_files[f], 13, 2))

    times[f] = julday(month, day, year, hour, minute, second)

    for s = 0L, n_stokes - 1L do begin
      if (strpos(line, stokes[s]) gt -1) then stokes_present[f] += stokes[s]
    endfor
  endfor
  free_lun, lun

  if (keyword_set(calibration)) then begin
    count = n_candidate_files
    return, candidate_files
  endif

  ; can't have a cluster of at least min_n_cluster_files if there aren't at
  ; least that many candidates
  if (n_candidate_files lt min_n_cluster_files) then begin
    count = 0L
    return, !null
  endif

  delta_time = times[1:*] - times[0:-2]

  time_check = delta_time lt max_cadence_interval

  bins = value_locate(times, flat_times)
  flat_check = histogram(bins, min=0, max=n_elements(time_check) - 1L) eq 0

  check = time_check and flat_check

  ; check must have at least 3 elements to work with LABEL_REGION, but if
  ; it doesn't have at least min_n_cluster_files we will return 0 files anyway
  if (n_elements(check) lt (3L > min_n_cluster_files)) then begin
    count = 0L
    stokes_present = []
    return, []
  endif

  clusters = label_region(check)
  if (check[0]) then clusters[0] = 1
  if (check[-1]) then clusters[-1] = max(clusters)

  for c = 1L, max(clusters) do begin
    ind = where(clusters eq c, n_cluster_intervals)
    n_files = n_cluster_intervals + 1L
    if (n_files ge min_n_cluster_files) then begin
      count = n_files < max_n_files
      stokes_present = (stokes_present[min(ind):max(ind) + 1])[0: count - 1]
      return, (candidate_files[min(ind):max(ind) + 1])[0:count - 1]
    endif
  endfor

  count = 0L
  stokes_present = []
  return, []
end


;+
; Find the files to be used by `COMP_AVERAGE`.
;
; :Returns:
;   `strarr` of filenames, returns `!null` if no files found
;
; :Params:
;   date_dir : in, required, type=string
;     date of date to process, i.e., '20160804'
;   wave_type : in, required, type=string
;     wavelength type, i.e., '1074', '1079', or '1083'
;
; :Keywords:
;   max_n_files : in, optional, type=integer, default=50
;     maximum number of files to be returned
;   min_n_cluster_files : in, optional, type=integer, default=40
;     minimum number of files needed in a cluster
;   max_cadence_interval : in, optional, type=float, default=180.0
;     time cadence (in seconds) to use to create clusters; files within a
;     cluster must be closer than `MAX_CADENCE_INTERVAL` apart
;   max_n_noncluster_files : in, optional, type=integer, default=50
;     maximum number of files to use if no cluster was good enough
;   stokes_present : out, optional, type=strarr
;     set to a named variable to retrieve the Stokes variables present in each
;     corresponding file
;   count : out, optional, type=integer
;     set to a named variable to retrieve the number of files returned
;   calibration : in, optional, type=boolean
;     set to indicate that files suitable for producing mean file for
;     calculting empirical crosstalk coefficients should be returned
;   synoptic : in, optional, type=boolean
;     set to use synoptic file instead of waves file
;-
function comp_find_average_files, date_dir, wave_type, $
                                  max_n_files=max_n_files, $
                                  min_n_cluster_files=min_n_cluster_files, $
                                  max_cadence_interval=max_cadence_interval, $
                                  max_n_noncluster_files=max_n_noncluster_files, $
                                  stokes_present=stokes_present, $
                                  count=count, $
                                  calibration=calibration, $
                                  synoptic=synoptic
  compile_opt strictarr
  @comp_config_common
  @comp_constants_common

  ; set defaults for optional keywords
  _max_cadence_interval = n_elements(max_cadence_interval) eq 0L $
                            ? 180.0 $
                            : max_cadence_interval
  _max_cadence_interval /= 60.0 * 60.0 * 24.0   ; convert seconds to days
  _min_n_cluster_files = n_elements(min_n_cluster_files) eq 0L $
                           ? 40L $
                           : min_n_cluster_files
  _max_n_files = n_elements(max_n_files) eq 0L ? 50L : max_n_files
  _max_n_noncluster_files = n_elements(max_n_noncluster_files) eq 0L $
                              ? 50L $
                              : max_n_noncluster_files

  count = 0L

  ; 0. if producing a calibration mean file, use the 5 points files in before
  ;    the second set of flats
  ; 1. using {date}.good.waves.{wave_type}.files.txt or
  ;    {date}.synoptic.{wave_type}.files.txt, group files into clusters where
  ;    files: 
  ;      - are within MAX_CADENCE_INTERVAL (3 min now) of each other
  ;      - are using the same flat
  ;    a. use the first cluster which has at least MIN_N_CLUSTER_FILES (40 now),
  ;       cutting it down to the first MAX_N_FILES (50 now) if it has more than
  ;       that

  ; 2. if step 1. didn't yield files, try it with {date}.good.iqu.{wave_type}.files.txt

  ; 3. if no files found yet, take the first files in
  ;    {date}.good.{wave_type}.files.txt up to MAX_N_NONCLUSTER_FILES (50 now)

  ; candidate filenames and their corresponding times
  l1_process_dir = filepath('level1', subdir=date_dir, root=process_basedir)

  flat_filename = filepath('flat.fts', root=l1_process_dir)
  if (~file_test(flat_filename)) then return, []
  fits_open, flat_filename, flat_fcb

  ; make sure there is a real flat.fts file with the 3 required extensions and
  ; at least one extension representing a flat
  if (size(flat_fcb, /type) ne 8 || flat_fcb.nextend lt 4) then return, []

  fits_read, flat_fcb, flat_times, flat_times_header, exten_no=flat_fcb.nextend - 2
  fits_close, flat_fcb

  year  = long(strmid(date_dir, 0, 4))
  month = long(strmid(date_dir, 4, 2))
  day   = long(strmid(date_dir, 6, 2))

  flat_times = julday(month, day, year, flat_times)

  flat_times += 10.0 / 24.0   ; adjust from local to UTC

  ; step 0.
  if (keyword_set(calibration)) then begin
    basename = string(date_dir, wave_type, format='(%"%s.good.all.%s.files.txt")')
    list_filename = filepath(basename, root=l1_process_dir)
    files = comp_find_average_files_findclusters(list_filename, $
                                                 date_dir=date_dir, $
                                                 wave_type=wave_type, $
                                                 stokes_present=stokes_present, $
                                                 count=count, $
                                                 /calibration)
    return, files
  endif

  ; step 1.
  if (keyword_set(synoptic)) then begin
    basename = string(date_dir, wave_type, format='(%"%s.synoptic.%s.files.txt")')
  endif else begin
    basename = string(date_dir, wave_type, format='(%"%s.good.waves.%s.files.txt")')
  endelse
  list_filename = filepath(basename, root=l1_process_dir)
  files = comp_find_average_files_findclusters(list_filename, flat_times, $
                                               max_cadence_interval=_max_cadence_interval, $
                                               min_n_cluster_files=_min_n_cluster_files, $
                                               max_n_files=_max_n_files, $
                                               stokes_present=stokes_present, $
                                               count=count)
  if (count gt 0L) then return, files

  ; step 2.
  basename = string(date_dir, wave_type, format='(%"%s.good.iqu.%s.files.txt")')
  list_filename = filepath(basename, root=l1_process_dir)
  files = comp_find_average_files_findclusters(list_filename, flat_times, $
                                               max_cadence_interval=_max_cadence_interval, $
                                               min_n_cluster_files=_min_n_cluster_files, $
                                               max_n_files=_max_n_files, $
                                               stokes_present=stokes_present, $
                                               count=count)
  if (count gt 0L) then return, files

  ; step 3.
  if (~file_test(list_filename)) then begin
    count = 0L
    return, !null
  endif

  n_candidate_files = file_lines(list_filename)
  if (n_candidate_files eq 0L) then begin
    count = 0L
    return, !null
  endif

  candidate_files = strarr(n_candidate_files)
  stokes_present = strarr(n_candidate_files)

  openr, lun, list_filename, /get_lun
  line = ''
  for f = 0L, n_candidate_files - 1L do begin
    readf, lun, line

    tokens = strsplit(line, /extract)
    candidate_files[f] = tokens[0]

    for s = 0L, n_stokes - 1L do begin
      if (strpos(line, stokes[s]) gt -1) then stokes_present[f] += stokes[s]
    endfor
  endfor
  free_lun, lun

  count = n_candidate_files < _max_n_noncluster_files
  stokes_present = stokes_present[0:count - 1L]
  return, candidate_files[0:count - 1L]
end
