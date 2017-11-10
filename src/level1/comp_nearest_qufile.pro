; docformat = 'rst'

;+
; Finds the nearest Stokes Q and U raw file to a specified CoMP raw file.
;
; :Returns:
;   string with name of nearest Q and U raw file
;
; :Uses:
;   comp_read_inventory_file, comp_constants_common, comp_config_common,
;   comp_mask_constants_common
;
; :Params:
;   date_dir : in, required, type=string
;     the date directory name
;   headers : in, required, type="fltarr(ntags, nimg)"
;     the headers corresponding to the input filename
;   filename: in, required, type=string
;
; :Author:
;   MLSO Software Team
;-
function comp_nearest_qufile, date_dir, headers, filename
  compile_opt strictarr

  @comp_constants_common
  @comp_config_common
  @comp_mask_constants_common

  wave_regions = [center1074, center1079, center1083]
  wave_types = ['1074', '1079', '1083']
  wl = sxpar(reform(headers[*, 0]), 'WAVELENG')
  !null = min(abs(wave_regions - wl), region_index)
  line = wave_types[region_index]

  ; find the inventory file and read it
  invenfile = filepath(line + '_files.txt', $
                       subdir=[date_dir, 'level1'], $
                       root=process_basedir)
  comp_read_inventory_file, invenfile, datafiles, exptimes, $
                            ndata, ndark, nopal, open, waves, polstates

  ; get the days and times from the filenames
  nfiles = n_elements(datafiles)
  if (nfiles eq '') then begin
    mg_log, 'no files in inventory file for wave type %s', line, /warn, $
            name='comp'
    return, ''
  endif
  files_split = strarr(nfiles, 3)
  for i = 0L, nfiles - 1L do begin
    files_split[i, *] = strsplit(datafiles[i], '.', /extract) 
  endfor
  days = files_split[*, 0]
  times = files_split[*, 1]

  ; require a file which has all of I+Q, I-Q, I+U, and I-U
  qucheck = total(polstates eq 'I+Q', 2) $
              and total(polstates eq 'I-Q', 2) $
              and total(polstates eq 'I+U', 2) $
              and total(polstates eq 'I-U', 2)

  ; find where our input filename lies in the list of data files:
  vindex = where(datafiles eq file_basename(filename))
  vindex = vindex[0]

  ; we want a Q and U file that has all the same wavelengths as our input file
  wavecheck = intarr(nfiles)
  for i = 0L, nfiles - 1L do begin
    wavecheck[i] = product(waves[i, *] eq waves[vindex, *])
  endfor

  ; lastly, we want the file that's closest in time
  quindices = where(qucheck and days eq days[vindex] and wavecheck)
  if (n_elements(times[quindices]) eq 1L) then begin
    ; this is a workaround for verions of IDL before 8.2.2 that can't
    ; handle 1-element arrays input into VALUE_LOCATE
    ind = ((times[vindex] ge times[quindices])[0]) - 1L
  endif else begin
    ind = value_locate(times[quindices], times[vindex])
  endelse
  qunearest = quindices[ind]

  qufile = filepath(datafiles[qunearest], subdir=date_dir, root=raw_basedir)
  return, qufile
end