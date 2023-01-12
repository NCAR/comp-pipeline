; docformat = 'rst'

;+
; Filter a array of stokes parameters for a particular stokes parameter.
;
; :Returns:
;   `lonarr`, indices into `stokes_present`, `!null` if no instances of the
;   given parameter
;
; :Params:
;   stokes_present : in, required, type=strarr
;     string array of the stokes parameters present in corresponding files,
;     i.e., ['iquv', 'iqu', 'iqu', 'iv']
;   stokes_parameter : in, required, type=string
;     stokes parameter to check for
;
; :Keywords:
;   count : out, optional, type=long
;     set to named variable to retrieve the number of indices returned
;-
function comp_find_average_files_filterbystokes, stokes_present, $
                                                 stokes_parameter, $
                                                 count=count
  compile_opt strictarr

  ; handle if stokes_present is null
  if (n_elements(stokes_present) eq 0L) then begin
    count = 0L
    return, !null
  endif

  mask = strpos(stokes_present, stokes_parameter) ge 0L
  return, where(mask, count, /null)
end


;+
; Return array of files in `list_filename`.
;
; :Returns:
;   `strarr`
;
; :Params:
;   list_filename : in, required, type=string
;     filename of candidate files
;
; :Keywords:
;   times : out, optional, type=dblarr
;     set to a named variable to retrieve the observation time of the files
;     listed in `list_filename`
;   stokes_present : out, optional, type=strarr
;     set to a named variable to retrieve the Stokes variables present in each
;     corresponding file
;   count : out, optional, type=long
;     set to a named variable to retrieve the number of files found
;-
function comp_find_average_files_inventory, list_filename, $
                                            times=times, $
                                            stokes_present=stokes_present, $
                                            count=count
  compile_opt strictarr
  @comp_constants_common

  if (~file_test(list_filename)) then begin
    mg_log, '%s does not exist', file_basename(list_filename), name='comp', /warn
    count = 0L
    return, !null
  endif

  ; determine number of files listed in inventory file...
  count = file_lines(list_filename)

  ; ...and exit if 0 files listed
  if (count eq 0L) then begin
    mg_log, 'no files in %s', file_basename(list_filename), name='comp', /warn
    return, !null
  endif

  ; otherwise, setup return variables
  files          = strarr(count)
  times          = dblarr(count)
  stokes_present = strarr(count)

  ; read the inventory file line by line to get L1 filename, Julian date, and
  ; Stokes parameters for each listed file
  openr, lun, list_filename, /get_lun
  line = ''
  for f = 0L, count - 1L do begin
    readf, lun, line

    tokens = strsplit(line, /extract)
    files[f] = tokens[1]

    year   = long(strmid(files[f],  0, 4))
    month  = long(strmid(files[f],  4, 2))
    day    = long(strmid(files[f],  6, 2))
    hour   = long(strmid(files[f],  9, 2))
    minute = long(strmid(files[f], 11, 2))
    second = long(strmid(files[f], 13, 2))

    times[f] = julday(month, day, year, hour, minute, second)

    for s = 0L, n_stokes - 1L do begin
      ; the letters I, Q, U, and V only appear in an inventory line if the
      ; corresponding Stokes parameter is listed
      if (strpos(line, stokes[s]) ge 0) then begin
        stokes_present[f] += strlowcase(stokes[s])
      endif
    endfor
  endfor
  free_lun, lun

  return, files
end


;+
; Find all the good files in the `list_filename` with the same flat.
;
; :Params:
;   list_filename : in, required, type=string
;     filename of candidate files
;   stokes_parameter : in, required, type=string
;     stokes parameter to find in files: 'i', 'q', 'u', or 'v'
;   flat_times : in, required, type=fltarr
;     times of the flats in Julian dates
;
; :Keywords:
;   count : out, optional, type=long
;     set to a named variable to retrieve the number of files found
;-
function comp_find_average_files_allgood, list_filename, $
                                          stokes_parameter, $
                                          flat_times, $
                                          count=count
  compile_opt strictarr
  @comp_constants_common

  mg_log, 'finding all good files for %s', stokes_parameter, name='comp', /debug

  ; do a basic inventory of the given files
  candidate_files = comp_find_average_files_inventory(list_filename, $
                                                      times=times, $
                                                      stokes_present=stokes_present, $
                                                      count=count)
  if (count eq 0L) then return, !null

  ; filter by Stokes parameter
  stokes_mask = strpos(stokes_present, stokes_parameter) ge 0L
  stokes_indices = where(stokes_mask, n_stokes_files)

  ; exit if no files with the given Stokes parameter...
  if (n_stokes_files eq 0L) then begin
    mg_log, 'found 0 all good files for %s', strupcase(stokes_parameter), $
            name='comp', /debug
    count = 0L
    return, !null
  endif

  ; ...otherwise use only files with the given Stokes parameter
  stokes_files = candidate_files[stokes_indices]
  stokes_times = times[stokes_indices]

  ; find files between each set of flats
  stokes_bins = value_locate(stokes_times, flat_times)
  
  stokes_start_indices = stokes_bins[uniq(stokes_bins)] + 1L
  before_ind = where(stokes_start_indices lt (n_elements(stokes_times) - 1L), $
                     ncomplement=n_after)
  if (n_after gt 0L) then stokes_start_indices = stokes_start_indices[before_ind]
  if (n_elements(stokes_start_indices) eq 1L) then begin
    stokes_end_indices = n_elements(stokes_times) - 1L
  endif else begin
    stokes_end_indices = [stokes_start_indices[1:-1], n_elements(stokes_times)] - 1L
  endelse
  files_per_flat = stokes_end_indices - stokes_start_indices + 1L

  ; return the largest set of files for a given flat
  count = max(files_per_flat, flat_index)
  if (count eq 0) then return, !null

  files = stokes_files[stokes_start_indices[flat_index]:stokes_end_indices[flat_index]]

  mg_log, 'found %d all good files for %s', $
          n_elements(files), strupcase(stokes_parameter), $
          name='comp', /debug

  return, files
end


;+
; Determines if the given `times` satisfy the cadence interval for a minimal
; number of files in a cluster. If so, cuts off the files at a given maximum
; number of files and returns the indices of the good files.
;
; :Returns:
;   1 for yes, 0 for no
;
; :Params:
;   times : in, required, type=dblarr
;     times in Julian day
;-
function comp_find_average_files_checkgaps, times, $
                                            min_n_cluster_files=min_n_cluster_files, $
                                            max_n_files=max_n_files, $
                                            cadence_interval=cadence_interval, $
                                            indices=indices
  compile_opt strictarr

  case n_elements(times) of
    0: return, 0B
    1: return, 1 ge min_n_cluster_files
    else:
  endcase

  gaps = times[1:-1] - times[0:-2]
  ok_gaps = [1B, gaps le cadence_interval]
  labels = comp_label_series(ok_gaps)

  for r = 1L, max(labels) do begin
    ind = where(labels eq r, count)
    if (count + 1L gt min_n_cluster_files) then begin
      indices = [ind - 1, max(ind)]

      ; make sure to only return at most MAX_N_FILES elements
      if (n_elements(indices) gt max_n_files) then begin
        mg_log, 'found %d files, cutting down to %d files', $
                n_elements(indices), max_n_files, $
                name='comp', /debug
        indices = indices[0:max_n_files - 1L]
      endif

      mg_log, 'found %d files with cadence of %0.1f sec', $
              n_elements(indices), cadence_interval, $
              name='comp', /debug

      return, 1B
    endif
  endfor

  ; didn't find a good cluster
  mg_log, 'found 0 files with cadence of %0.1f sec', $
          cadence_interval, $
          name='comp', /debug
  indices = !null
  return, 0B
end


;+
; Find all the good files in the `list_filename` with the same flat.
;
; :Params:
;   list_filename : in, required, type=string
;     filename of candidate files
;   stokes_parameter : in, required, type=string
;     stokes parameter to find in files: 'i', 'q', 'u', or 'v'
;   flat_times : in, required, type=fltarr
;     times of the flats in Julian dates
;
; :Keywords:
;   count : out, optional, type=long
;     set to a named variable to retrieve the number of files found
;-
function comp_find_average_files_nogap, list_filename, $
                                        stokes_parameter, $
                                        flat_times, $
                                        min_n_cluster_files=min_n_cluster_files, $
                                        min_n_files=min_n_files, $
                                        max_n_files=max_n_files, $
                                        cadence_interval=cadence_interval, $
                                        stokes_present=stokes_present, $
                                        count=count
  compile_opt strictarr
  @comp_constants_common

  ; do a basic inventory of the given files
  candidate_files = comp_find_average_files_inventory(list_filename, $
                                                      times=times, $
                                                      stokes_present=stokes_present, $
                                                      count=count)
  if (count eq 0L) then return, !null

  ; filter by stokes parameter
  stokes_mask = strpos(stokes_present, stokes_parameter) ge 0L
  stokes_indices = where(stokes_mask, n_stokes_files)

  ; exit if no files with the given stokes parameter...
  if (n_stokes_files eq 0L) then begin
    mg_log, 'no files for %s', stokes_parameter, name='comp', /debug
    count = 0L
    return, !null
  endif

  ; ...otherwise use only files with the given stokes parameter
  stokes_files = candidate_files[stokes_indices]
  stokes_times = times[stokes_indices]
  stokes_stokes_present = stokes_present[stokes_indices]

  ; find files between each set of flats
  stokes_bins = value_locate(stokes_times, flat_times)

  stokes_start_indices   = stokes_bins[uniq(stokes_bins)] + 1L
  before_ind = where(stokes_start_indices lt n_elements(stokes_times) - 1L, $
                     ncomplement=n_after)
  if (n_after gt 0L) then stokes_start_indices = stokes_start_indices[before_ind]
  if (n_elements(stokes_start_indices) eq 1L) then begin
    stokes_end_indices = n_elements(stokes_times) - 1L
  endif else begin
    stokes_end_indices = [stokes_start_indices[1:-1], n_elements(stokes_times)] - 1L
  endelse
  files_per_flat = stokes_end_indices - stokes_start_indices + 1L

  count = max(files_per_flat)
  if (count eq 0L) then return, !null

  ; loop through the flats
  for flat_index = 0L, n_elements(files_per_flat) - 1L do begin
    s = stokes_start_indices[flat_index]
    e = stokes_end_indices[flat_index]
    perflat_files = stokes_files[s:e]
    perflat_times = stokes_times[s:e]
    perflat_stokes_present = stokes_stokes_present[s:e]

    mg_log, 'checking images with flat index %d', flat_index, $
            name='comp', /debug
    cluster_found = comp_find_average_files_checkgaps(perflat_times, $
                                                      min_n_cluster_files=min_n_cluster_files, $
                                                      max_n_files=max_n_files, $
                                                      cadence_interval=cadence_interval, $
                                                      indices=cluster_indices)
    if (cluster_found) then begin
      mg_log, 'found a cluster with cadence interval %0.1f sec', $
              cadence_interval, $
              name='comp', /debug

      files = perflat_files[cluster_indices]
      count = n_elements(files)
      stokes_present = perflat_stokes_present[cluster_indices]

      goto, found
    endif
  endfor

  mg_log, 'did''t find a cluster', name='comp', /debug
  mg_log, 'looking for at least %d files', min_n_files, $
          name='comp', /debug

  ; didn't find a cluster: are there min_n_files?
  for flat_index = 0L, n_elements(files_per_flat) - 1L do begin
    s = stokes_start_indices[flat_index]
    e = stokes_end_indices[flat_index]
    if (e - s + 1 ge min_n_files) then begin
      files = stokes_files[s:e]
      count = n_elements(files)
      stokes_present = stokes_stokes_present[s:e]

      mg_log, 'found %d non-clustered files', count, name='comp', /debug

      goto, found
    endif
  endfor

  ; didn't find the minimum number of files
  mg_log, 'didn''t find at least %d files', min_n_files, name='comp', /debug

  count = 0L
  stokes_present = !null
  return, !null

  found:

  return, files
end


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
        if (count gt 0L) then new_chosen[ind[- n_to_remove:*]] = 0B
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
                                  qu_files=qu_files, $
                                  v_files=v_files
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

  fits_read, flat_fcb, flat_times, flat_times_header, exten_no=flat_fcb.nextend - 2, $
             /no_abort, message=msg
  if (msg ne '') then message, msg
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
    basename = string(date_dir, wave_type, format='(%"%s.comp.%s.good.waves.files.txt")')
  endelse

  mg_log, '%s', basename, name='comp', /debug
  list_filename = filepath(basename, root=l1_process_dir)

  if (keyword_set(synoptic)) then begin
    qu_files = comp_find_average_files_allgood(list_filename, 'q', flat_times)
    v_files  = comp_find_average_files_allgood(list_filename, 'v', flat_times)

    mg_log, 'found %d QU good files and %d V good files with the same flat', $
            n_elements(qu_files), n_elements(v_files), $
            name='comp', /debug

    max_n_files = averaging_max_n_synoptic_files
    min_n_files = averaging_min_n_qu_synoptic_files

    for min_c = 0L, n_elements(averaging_min_n_cluster_synoptic_files) - 1L do begin
      min_n_cluster_files = averaging_min_n_cluster_synoptic_files[min_c]
      for mci = 0L, n_elements(averaging_max_cadence_interval) - 1L do begin
        cadence_interval = averaging_max_cadence_interval[mci]

        mg_log, 'searching for >= %d I files with cadence %0.1f sec', $
                min_n_cluster_files, cadence_interval, $
                name='comp', /debug

        ; convert to days
        cadence_interval /= 60.0D * 60.0D * 24.0D

        i_files = comp_find_average_files_nogap(list_filename, 'i', flat_times, $
                                                min_n_cluster_files=min_n_cluster_files, $
                                                min_n_files=min_n_files, $
                                                max_n_files=max_n_files, $
                                                cadence_interval=cadence_interval, $
                                                count=n_i_files)
        if (n_i_files gt 0L) then begin
          mg_log, 'found %d I files', n_i_files, name='comp', /debug

          goto, synoptic_found
        endif
      endfor
    endfor

    count = 0L
    return, !null

    synoptic_found:
    count = n_elements(i_files) + n_elements(qu_files) + n_elements(v_files)

    return, i_files
  endif else begin
    max_n_files = averaging_max_n_waves_files
    min_n_files = averaging_min_n_qu_waves_files

    for min_c = 0L, n_elements(averaging_min_n_cluster_synoptic_files) - 1L do begin
      min_n_cluster_files = averaging_min_n_cluster_waves_files[min_c]
      for mci = 0L, n_elements(averaging_max_cadence_interval) - 1L do begin
        cadence_interval = averaging_max_cadence_interval[mci]
        ;cadence_interval /= 60.0D * 60.0D * 24.0D
        mg_log, 'searching for at least %d I files with cadence interval %0.1f sec', $
                min_n_cluster_files, cadence_interval, $
                name='comp', /debug

        i_files = comp_find_average_files_nogap(list_filename, 'i', flat_times, $
                                                min_n_cluster_files=min_n_cluster_files, $
                                                min_n_files=min_n_files, $
                                                max_n_files=max_n_files, $
                                                cadence_interval=cadence_interval, $
                                                stokes_present=stokes_present, $
                                                count=n_i_files)
        if (n_i_files gt 0L) then begin
          mg_log, 'found %d I files', n_i_files, name='comp', /debug

          goto, waves_found
        endif
      endfor
    endfor

    count = 0L
    return, !null

    waves_found:

    ; filter I files for QU and V

    qu_indices = comp_find_average_files_filterbystokes(stokes_present, 'q')
    v_indices  = comp_find_average_files_filterbystokes(stokes_present, 'v')

    mg_log, 'filtered %d I files down to %d QU files', $
            n_i_files, n_elements(qu_indices), $
            name='comp', /debug
    mg_log, 'filtered %d I files down to %d V files', $
            n_i_files, n_elements(v_indices), $
            name='comp', /debug

    qu_files = i_files[qu_indices]
    v_files  = i_files[v_indices]

    count = n_i_files

    return, i_files
  endelse
end


; main-level example program

;dates = ['20171001', '20171002', '20171003', '20171004', '20171005', $
;         '20171006', '20171007', '20171008', '20171009', '20171010', $
;         '20171011', '20171012', '20171013', '20171014']
dates = ['20121203']
wave_type = '1079'

config_filename = filepath('comp.reprocess.cfg', $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())
comp_configuration, config_filename=config_filename

for d = 0L, n_elements(dates) - 1L do begin
  comp_update_configuration, dates[d]
  comp_initialize, dates[d]

  synoptic_i_files = comp_find_average_files(dates[d], wave_type, $
                                             qu_files=synoptic_qu_files, $
                                             v_files=synoptic_v_files, $
                                             count=n_synoptic_files, $
                                             /synoptic)

  if (n_synoptic_files gt 0L) then begin
    print, n_elements(synoptic_i_files), format='(%"%d synoptic I files")'
    print, n_elements(synoptic_qu_files), format='(%"%d synoptic QU files")'
    print, n_elements(synoptic_v_files), format='(%"%d synoptic V files")'
  endif else print, 'no synoptic I files'

  waves_i_files = comp_find_average_files(dates[d], wave_type, $
                                          qu_files=waves_qu_files, $
                                          v_files=waves_v_files, $
                                          count=n_waves_files)

  if (n_waves_files gt 0L) then begin
    print, n_elements(waves_i_files), format='(%"%d waves I files")'
    print, n_elements(waves_qu_files), format='(%"%d waves QU files")'
    print, n_elements(waves_v_files), format='(%"%d waves V files")'
  endif else print, 'no waves I files'
endfor

end
