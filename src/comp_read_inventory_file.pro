; docformat = 'rst'

;+
; Reads a CoMP inventory file and returns its contents in a set of named
; variables. Only works for CoMP raw data inventory files (assumes polarization
; states are I+Q, etc). Assumes inventory file lines are the following format::
;
;   20150503.064434.FTS     250. ms      0 Data    39 Dark     0 Opal   CLOSED  1074.70  I+Q
;
; :Params:
;   filename: in, required, type=string
;     the name of the inventory file to read
;   datafiles: out, required, type=strarr(nlines)
;     the names of each data file found in the inventory file
;   exptimes: out, required, type=strarr(nlines)
;     the exposure times for each data file found in the inventory file
;   ndata: out, required, type=lonarr(nlines)
;     the number of data images in each data file
;   ndark: out, required, type=lonarr(nlines)
;     the number of dark images in each data file
;   nopal: out, required, type=lonarr(nlines)
;     the number of flat images in each data file
;   open: out, required, type=strarr(nlines)
;     'OPEN' or 'CLOSED' instrument state, for each data file
;   waves: out, required, type="strarr(nlines, nwmax)"
;     string array listing each wavelength in each file; if some files have
;     fewer wavelengths than others, their trailing entries in the array will
;     be left blank
;   polstates: out, optional, type="strarr(nlines, npsmax)"
;     string array listing each polarization state in each file; ff some files
;      have fewer polarizations than others their trailing entries in the array
;      will be left blank
;
; :Author:
;   Joseph Plowman
;-
pro comp_read_inventory_file, filename, datafiles, exptimes, $
                              ndata, ndark, nopal, $
                              open, waves, polstates
  compile_opt strictarr

  openr, lun, filename, /get_lun
  line = ''

  ; all possible (raw) polarization states:
  polstates = ['I+Q', 'I-Q', 'I+U', 'I-U', 'I+V', 'I-V']
  npstates = n_elements(polstates)
  nwmax = 0
  npsmax = 0

  ; standard inventory file will have wavelengths begin at 10th
  ; space-delimited field
  iwave0 = 10

  ; read each line of the file...
  while (~eof(lun)) do begin
    readf, lun, line 
    linesplit = strtrim(strsplit(line, /extract))
    nls = n_elements(linesplit)

    ; find how many polarization states are present...
    npol = 0
    for i = 0L, npstates - 1L do npol += total(linesplit eq polstates[i])
    if (npol gt npsmax) then npsmax = npol

    ; elements from iwave0 to the end of linesplit will be wavelengths
    ; and polarizations, so the number of wavelengths are the following...
    nw = nls - npol - iwave0
    if (nw gt nwmax) then nwmax = nw
    if (n_elements(lines) eq 0) then begin
      lines = line
      npols = npol
      nws = nw
    endif else begin
      lines = [lines, line]
      npols = [npols, npol]
      nws = [nws, nw]
    endelse
  endwhile

  ; set up output arrays...
  nlines    = n_elements(lines)
  datafiles = strarr(nlines)
  exptimes  = strarr(nlines)
  ndata     = lonarr(nlines)
  ndark     = lonarr(nlines)
  nopal     = lonarr(nlines)
  open      = strarr(nlines)
  waves     = strarr(nlines, nwmax)
  polstates = strarr(nlines, npsmax)

  ; go through the lines again and assign to output
  for i = 0L, nlines - 1L do begin
    linesplit = strtrim(strsplit(lines[i], /extract))
    nls = n_elements(linesplit)
    datafiles[i] = linesplit[0]
    exptimes[i] = linesplit[1]
    ndata[i] = long(linesplit[3])
    ndark[i] = long(linesplit[5])
    nopal[i] = long(linesplit[7])
    open[i] = linesplit[9]
    waves[i, 0:nws[i] - 1] = linesplit[iwave0:iwave0 + nws[i] - 1]
    polstates[i, 0:npols[i] - 1] = linesplit[nls - npols[i]:nls - 1]
  endfor

  free_lun, lun
end
