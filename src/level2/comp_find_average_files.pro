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
;     filename of candidate files, in the format of good_{wave_type}_files.txt, etc.
;   flat_times : in, required, type=fltarr
;     times of the flats in hours from local midnight
;
; :Keywords:
;   max_cadence_interval : in, required, type=float
;     max time allowed between files before starting a new cluster, in days
;   max_n_files : in, required, type=long
;     maximum number of files to be returned
;   min_n_cluster_files : in, required, type=long
;     minimum number of files needed in a cluster
;   count : out, optional, type=long
;     set to a named variable to retrieve the number of files found
;-
function comp_find_average_files_findclusters, list_filename, flat_times, $
                                               max_cadence_interval=max_cadence_interval, $
                                               min_n_cluster_files=min_n_cluster_files, $
                                               max_n_files=max_n_files, $
                                               count=count
  compile_opt strictarr

  n_candidate_files = file_lines(list_filename)
  if (n_candidate_files eq 0L) then begin
    count = 0L
    return, !null
  endif

  candidate_files = strarr(n_candidate_files)
  times = dblarr(n_candidate_files)

  openr, lun, list_filename, /get_lun
  line = ''
  for f = 0L, n_candidate_files - 1L do begin
    readf, lun, line

    tokens = strsplit(line, /extract)
    candidate_files[f] = tokens[0]

    year   = long(strmid(line,  0, 4))
    month  = long(strmid(line,  4, 2))
    day    = long(strmid(line,  6, 2))
    hour   = long(strmid(line,  9, 2))
    minute = long(strmid(line, 11, 2))
    second = long(strmid(line, 13, 2))

    times[f] = julday(month, day, year, hour, minute, second)
  endfor
  free_lun, lun

  delta_time = times[1:*] - times[0:-2]

  time_check = delta_time lt max_cadence_interval

  bins = value_locate(times, flat_times)
  flat_check = histogram(bins, min=0, max=n_elements(time_check) - 1L) eq 0

  check = time_check and flat_check

  clusters = label_region(check)
  if (check[0]) then clusters[0] = 1
  if (check[-1]) then clusters[-1] = max(clusters)

  for c = 1L, max(clusters) do begin
    ind = where(clusters eq c, n_cluster_intervals)
    n_files = n_cluster_intervals + 1L
    if (n_files ge min_n_cluster_files) then begin
      count = n_files < max_n_files
      return, (candidate_files[min(ind):max(ind) + 1])[0:count - 1]
    endif
  endfor

  count = 0L
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
;   max_n_files : in, optional, type=integer, default=150
;     maximum number of files to be returned
;   min_n_cluster_files : in, optional, type=integer, default=50
;     minimum number of files needed in a cluster
;   max_cadence_interval : in, optional, type=float, default=180.0
;     time cadence (in seconds) to use to create clusters; files within a
;     cluster must be closer than `MAX_CADENCE_INTERVAL` apart
;   max_n_noncluster_files : in, optional, type=integer, default=50
;     maximum number of files to use 
;   count : out, optional, type=integer
;     set to a named variable to retrieve the number of files returned
;-
function comp_find_average_files, date_dir, wave_type, $
                                  max_n_files=max_n_files, $
                                  min_n_cluster_files=min_n_cluster_files, $
                                  max_cadence_interval=max_cadence_interval, $
                                  max_n_noncluster_files=max_n_noncluster_files, $
                                  count=count
  compile_opt strictarr
  @comp_config_common

  ; set defaults for optional keywords
  _max_cadence_interval = n_elements(max_cadence_interval) eq 0L $
                            ? 180.0 $
                            : max_cadence_interval
  _max_cadence_interval /= 60.0 * 60.0 * 24.0   ; convert seconds to days
  _min_n_cluster_files = n_elements(min_n_cluster_files) eq 0L $
                           ? 50L $
                           : min_n_cluster_files
  _max_n_files = n_elements(max_n_files) eq 0L ? 150L : max_n_files

  ; 1. using good_waves_{wave_type}_files.txt, group files into clusters where
  ;    files: 
  ;      - are within MAX_CADENCE_INTERVAL (3 min now) of each other
  ;      - are using the same flat
  ;    a. use the first cluster which has at least MIN_N_CLUSTER_FILES (50 now),
  ;       cutting it down to the first MAX_N_FILES (150 now) if it has more than
  ;       that
  ; 2. if step 1. didn't yield files, try it with good_{wave_type}_files.txt
  ; 3. if no files found yet, take all the files in the first clusters which
  ;    keep the total number of files less than MAX_N_NONCLUSTER_FILES (50 now)

  cd, current=process_dir

  ; candidate filenames and their corresponding times
  l1_process_dir = filepath('level1', root=process_dir)

  flat_filename = filepath('flat.fts', root=l1_process_dir)
  fits_open, flat_filename, flat_fcb
  fits_read, flat_fcb, flat_times, flat_times_header, exten_no=flat_fcb.nextend - 2
  fits_close, flat_fcb

  year  = long(strmid(date_dir, 0, 4))
  month = long(strmid(date_dir, 4, 2))
  day   = long(strmid(date_dir, 6, 2))

  flat_times = julday(month, day, year, flat_times)

  flat_times += 10.0 / 24.0   ; adjust from local to UTC

  ; step 1.
  basename = string(wave_type, format='(%"good_waves_%s_files.txt")')
  list_filename = filepath(basename, root=l1_process_dir)
  files = comp_find_average_files_findclusters(list_filename, flat_times, $
                                               max_cadence_interval=_max_cadence_interval, $
                                               min_n_cluster_files=_min_n_cluster_files, $
                                               max_n_files=_max_n_files, $
                                               count=count)
  if (count gt 0L) then return, files

  ; step 2.
  basename = string(wave_type, format='(%"good_%s_files.txt")')
  list_filename = filepath(basename, root=l1_process_dir)
  files = comp_find_average_files_findclusters(list_filename, flat_times, $
                                               max_cadence_interval=_max_cadence_interval, $
                                               min_n_cluster_files=_min_n_cluster_files, $
                                               max_n_files=_max_n_files, $
                                               count=count)
  if (count gt 0L) then return, files

  ; step 3.

  return, []
end
