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
;   min_n_qu_files : in, required, type=long
;     minimum number of QU files needed
;   stokes_present : out, optional, type=strarr
;     set to a named variable to retrieve the Stokes variables present in each
;     corresponding file
;   count : out, optional, type=long
;     set to a named variable to retrieve the number of files found
;   calibration : in, optional, type=boolean
;     set to indicate that files suitable for producing mean file for
;     calculting empirical crosstalk coefficients should be returned
;   synoptic : in, optional, type=boolean
;     set to perform a synoptic averaging
;
; :Author:
;   MLSO Software Team
;-
function comp_find_average_files_findclusters, list_filename, flat_times, $
                                               date_dir=date_dir, wave_type=wave_type, $
                                               max_cadence_interval=max_cadence_interval, $
                                               min_n_cluster_files=min_n_cluster_files, $
                                               min_n_qu_files=min_n_qu_files, $
                                               max_n_files=max_n_files, $
                                               stokes_present=stokes_present, $
                                               count=count, $
                                               calibration=calibration, $
                                               synoptic=synoptic
  compile_opt strictarr
  @comp_constants_common

  if (~file_test(list_filename)) then begin
    mg_log, '%s does not exist', file_basename(list_filename), name='comp', /warn
    count = 0L
    return, !null
  endif

  n_candidate_files = file_lines(list_filename)

  if (n_candidate_files eq 0L) then begin
    mg_log, 'no files in %s', file_basename(list_filename), name='comp', /warn
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
    mg_log, '%d candidate files < MIN_N_CLUSTER_FILES (%d)', $
            n_candidate_files, $
            min_n_cluster_files, $
            name='comp', /warn

    count = 0L
    return, !null
  endif

  delta_time = times[1:*] - times[0:-2]

  time_check = [1B, delta_time lt max_cadence_interval]

  flat_bins = value_locate(times, flat_times) + 1L
  flat_check = histogram(flat_bins, min=0, max=n_elements(time_check) - 1L) eq 0

  check = flat_check and time_check

  if (keyword_set(synoptic)) then begin
    qu_mask = strpos(stokes_present, 'Q') ge 0L and strpos(stokes_present, 'U') ge 0L
    mg_log, '%d QU files found', total(qu_mask, /integer), name='comp', /debug
    if (total(qu_mask, /integer) lt min_n_qu_files) then begin
      mg_log, '%d QU files < MIN_N_QU_FILES (%d)', $
              total(qu_mask, /integer), $
              min_n_qu_synoptic_files, $
              name='comp', /warn

      count = 0L
      return, !null
    endif
  endif

  ; check must have at least 3 elements to work with LABEL_REGION, but if
  ; it doesn't have at least min_n_cluster_files we will return 0 files anyway
  if (n_elements(check) lt (3L > min_n_cluster_files)) then begin
    mg_log, '%d candidate files < max(3, MIN_N_CLUSTER_FILES) (%d)', $
            n_elements(check), $
            3L > min_n_cluster_files, $
            name='comp', /warn

    count = 0L
    stokes_present = []
    return, []
  endif

  clusters = label_region(check)
  if (check[-1]) then clusters[-1] = max(clusters)

  flat_clusters = label_region(flat_check)
  for b = 0L, n_elements(flat_bins) - 1L do begin
    if (flat_bins[b] lt n_elements(flat_clusters) - 1L) then begin
      flat_clusters[flat_bins[b]] = (flat_bins[b] + 1) lt n_elements(flat_clusters) - 1L $
                                      ? flat_clusters[flat_bins[b] + 1] $
                                      : (max(flat_clusters[0:-2]) + 1)
    endif
  endfor

  for c = 1L, max(clusters) do begin
    ind = where(clusters eq c, n_cluster_intervals)

    chosen = bytarr(n_candidate_files)
    chosen[min(ind):max(ind)] = 1B

    ; if synoptic, then add QU in the same flat even if not inside cadence
    if (keyword_set(synoptic)) then begin
      qu_flat_mask = (flat_clusters eq flat_clusters[min(ind)]) and qu_mask
      n_qu_flat_files = total(qu_flat_mask, /integer)
      if (n_qu_flat_files lt min_n_qu_files) then continue
      new_chosen = chosen or qu_flat_mask

      n_files = total(new_chosen, /integer)
      if (n_files gt max_n_files) then begin
        ; remove last non-QU files
        n_to_remove = n_files - max_n_files
        ind = where(chosen and not qu_flat_mask, count)
        new_chosen[ind[- n_to_remove:*]] = 0B
      endif
      chosen = new_chosen
    endif

    n_files = total(chosen, /integer)

    if (n_files ge min_n_cluster_files) then begin
      count = n_files < max_n_files
      chosen_ind = (where(chosen))[0:count - 1L]
      stokes_present = stokes_present[chosen_ind]
      return, candidate_files[chosen_ind]
    endif
  endfor

  mg_log, 'no files to average', name='comp', /warn

  count = 0L
  l1_filenames = !null
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
;   combined : in, optional, type=boolean
;     set to use iqu file instead of waves file
;-
function comp_find_average_files, date_dir, wave_type, $
                                  stokes_present=stokes_present, $
                                  count=count, $
                                  calibration=calibration, $
                                  synoptic=synoptic, $
                                  combined=combined
  compile_opt strictarr
  @comp_config_common
  @comp_constants_common

  count = 0L

  ; 0. if producing a calibration mean file, use the 5 points files in before
  ;    the second set of flats
  ; 1. using {date}.comp.{wave_type}.good.waves.files.txt or
  ;    {date}.comp.{wave_type}.good.synoptic.files.txt, group files into
  ;    clusters where files: 
  ;      - are within MAX_CADENCE_INTERVAL (3 min now) of each other
  ;      - are using the same flat
  ;    a. use the first cluster which has at least MIN_N_CLUSTER_FILES (40 now),
  ;       cutting it down to the first MAX_N_FILES (50 now) if it has more than
  ;       that

  ; only if COMBINED is set:
  ; 2. if step 1. didn't yield files, try it with {date}.good.iqu.{wave_type}.files.txt

  ; candidate filenames and their corresponding times
  l1_process_dir = filepath('level1', subdir=date_dir, root=process_basedir)

  flat_filename = filepath(string(date_dir, format='(%"%s.comp.flat.fts")'), $
                           root=l1_process_dir)
  if (~file_test(flat_filename)) then return, []

  fits_open, flat_filename, flat_fcb

  ; make sure there is a real flat FITS file with the 3 required extensions and
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
    basename = string(date_dir, wave_type, format='(%"%s.comp.%s.good.all.files.txt")')
    mg_log, 'CALIBRATION set, using %s', basename, name='comp', /debug

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
    basename = string(date_dir, wave_type, format='(%"%s.comp.%s.good.synoptic.files.txt")')
  endif else begin
    if (keyword_set(combined)) then begin
      basename = string(date_dir, wave_type, format='(%"%s.comp.%s.good.iqu.files.txt")')
    endif else begin
      basename = string(date_dir, wave_type, format='(%"%s.comp.%s.good.waves.files.txt")')
    endelse
  endelse

  mg_log, 'trying %s', basename, name='comp', /debug
  list_filename = filepath(basename, root=l1_process_dir)

  for mci = 0L, n_elements(averaging_max_cadence_interval) - 1L do begin
    interval = averaging_max_cadence_interval[mci]
    ; convert to seconds to days
    interval /= 60.0D * 60.0D * 24.0D
    case 1 of
      keyword_set(synoptic): begin
          max_n_files = averaging_max_n_synoptic_files
          min_n_cluster_files = averaging_min_n_cluster_synoptic_files
          min_n_qu_files = averaging_min_n_qu_synoptic_files
        end
      keyword_set(combined): begin
          max_n_files = averaging_max_n_combined_files
          min_n_cluster_files = averaging_min_n_cluster_synoptic_files
          min_n_qu_files = averaging_min_n_qu_synoptic_files
        end
      else: begin
          max_n_files = averaging_max_n_waves_files
          min_n_cluster_files = averaging_min_n_cluster_waves_files
          min_n_qu_files = averaging_min_n_qu_waves_files
        end
    endcase

    files = comp_find_average_files_findclusters(list_filename, flat_times, $
                                                 max_cadence_interval=interval, $
                                                 min_n_cluster_files=min_n_cluster_files, $
                                                 min_n_qu_files=min_n_qu_files, $
                                                 max_n_files=max_n_files, $
                                                 stokes_present=stokes_present, $
                                                 synoptic=synoptic, $
                                                 count=count)
    if (count gt 0L) then begin
      mg_log, 'found using max cadence of %0.1f sec', interval, $
              name='comp', /debug
      return, files
    endif
  endfor

  ; failed to find any files
  count = 0L
  return, !null
end


; main-level example program

;dates = ['20171001', '20171002', '20171003', '20171004', '20171005', $
;         '20171006', '20171007', '20171008', '20171009', '20171010', $
;         '20171011', '20171012', '20171013', '20171014']
dates = ['20171001']

for d = 0L, n_elements(dates) - 1L do begin
  comp_initialize, dates[d]
  comp_configuration, config_filename='../../config/comp.mgalloy.mahi.latest.cfg'

  synoptic_files = comp_find_average_files(dates[d], '1074', $
                                           count=n_synoptic_files, /synoptic)
;  waves_files = comp_find_average_files(dates[d], '1074', $
;                                        count=n_waves_files)
;  combined_files = comp_find_average_files(dates[d], '1074', $
;                                           count=n_combined_files, $
;                                           /combined)

;  print, dates[d], n_synoptic_files, n_waves_files, n_combined_files, $
;         format='(%"---> %s: %d synoptic, %d waves, %d combined")'
endfor

end
