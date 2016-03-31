; docformat = 'rst'

;+
; :Examples:
;   For example, call like::
;
;     cd, '/hao/kaula1/Data/CoMP/process/20120606'
;     gbu = comp_read_gbu('GBU.1079.log')
;
; :Params:
;    gbu_file
;
; :Keywords:
;    verbose : in, optional, type=boolean
;
; :Author:
;   Christian Bethge
;
; :History:
;   removed gzip    Oct 1 2014  GdT
;-
function comp_read_gbu, gbu_file, verbose=verbose
  compile_opt strictarr

  nlines = file_lines(gbu_file)
  sarr = strarr(nlines)
  openr, unit, gbu_file, /get_lun
  readf, unit, sarr
  free_lun, unit

  mg_log, 'CoMP GBU file has %d entries', nlines - 1, name='comp', /debug

  for ii = 1L, n_elements(sarr) - 1L do begin
    str = {l1file:'', $
           time_obs:'', $
           quality:'', $
           background:0., $
           variance:0., $
           wavelengths:0}
    x = str_sep(sarr[ii], ' ')
    best = where(x ne '', bc)
    x = x[best]
    str.l1file = x[0]

    ;ttt = str_sep(x[0],'.gz')
    ;file = ttt[0]
    file = x[0]
    ttt = str_sep(file, '.fts')
    ttt = str_sep(ttt[0], '/')
    base = ttt[n_elements(ttt) - 1]

    ttt = str_sep(x[0], '.')
    time = strmid(ttt[0], 0, 4) $
             + '-' + strmid(ttt[0], 4, 2) $
             + '-' + strmid(ttt[0], 6, 2) $
             + ' ' + strmid(ttt[1], 0, 2) $
             + ':' + strmid(ttt[1], 2, 2) $
             + ':' + strmid(ttt[1], 4, 2)
    str.time_obs = time

    ; different generations of GBU file format
    if (n_elements(x) eq 4) then begin
      str.quality = x[1]
      str.background = 0.
      str.variance = float(x[2])
      str.wavelengths = fix(x[3])
    endif else begin
      str.quality = x[1]
      str.background = float(x[2])
      str.variance = float(x[3])
      str.wavelengths = fix(x[4])
    endelse

    ;   ofile = base+'.FitI.'+fns('#',str.wavelengths)+'.sav'
    ;   str.l2file = ofile

    if (ii eq 0) then gbu = str
    if (ii gt 0) then gbu = merge_struct(gbu, str)
  endfor

  mg_log, name='comp', logger=logger
  logger->getProperty, level=level
  if (level eq 5) then begin  ; 5 = debug
    good = where(gbu.quality eq 'Good', ngf)
    mg_log, 'Directory file has %d Good images', ngf, name='comp', /debug

    nfive = where(gbu.quality eq 'Good' and gbu.wavelengths eq 5, ng5)
    nthree = where(gbu.quality eq 'Good' and gbu.wavelengths eq 3, ng3)
    mg_log, '...of those, there are %d 5pt and %d 3pt measurements', $
            ng5, ng3, name='comp', /debug

    bad = where(gbu.quality eq 'Bad', nbf)
    mg_log, 'Directory file has %d Bad images', nbf, name='comp', /debug

    ugly = where(gbu.quality eq 'Ugly', nuf)
    mg_log, 'Directory file has %d Ugly images', nuf, name='comp', /debug
  endif

  return, gbu
end
