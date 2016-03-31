; docformat = 'rst'

;+
; :Params:
;   in_header
;
; :Keywords:
;    exten
;    extname
;    datminmax
;
; :Author: Christian Bethge
;-
function comp_convert_header, in_header, exten=exten, extname=extname, $
                              datminmax=datminmax
  compile_opt strictarr

  main = fitshead2struct(in_header)

  if (not keyword_set(exten)) then begin
    rtags = ['NTUNE', 'TNELNGTH', 'TUNEDLAY', 'H_D$OCCULT', 'V_D$OCCULT', $
             'FOCUS', 'COVER', 'POLANGLE', 'POLARIZR', 'OPAL', 'RETARDER', $
             'OXCNTER1', 'OYCNTER1', 'OXCNTER2', 'OYCNTER2', 'FXCNTER1', $
             'FYCNTER1', 'FXCNTER2', 'FYCNTER2', 'METHOD', 'OCRAD1', $
             'OCRAD2', 'FCRAD1', 'FCENX1', 'FCENY1', 'FCRAD2', 'FCENX2', $
             'FCENY2', 'CRRADIUS', 'OCC_D$ID', 'OCC_D$SIZE']

    for ii = 0L, n_elements(rtags) - 1L do main = rem_tag(main, rtags[ii])

    time_obs = main.date_hst + ' ' + main.time_hst
    main.DATE_HST = time_obs
    out_header = struct2fitshead(main)
    sxaddpar, out_header, 'LEVEL',  'L2', ' '
    sxaddpar, out_header, 'EXTEND', 'T', 'file may contain extensions', $
              after='NAXIS'
  endif else begin
    rtags = ['POLSTATE', 'NAVERAGE', 'FILTER', 'PCOUNT', 'GCOUNT', 'DATATYPE', $
             'BODYTEMP', 'BASETEMP', 'RACKTEMP', 'EXPOSURE', 'OPTRTEMP', $
             'DEMULT', 'FILTTEMP', 'FITMNLIN', 'FITVRLIN', 'FLATFILE']

    for ii = 0L, n_elements(rtags) - 1L do main = rem_tag(main, rtags[ii])

    out_header = struct2fitshead(main)
    sxaddpar, out_header, 'XTENSION', 'IMAGE', 'extension type', before='BITPIX'
    sxaddpar, out_header, 'EXTNAME',  extname, before='BITPIX'
    sxaddpar, out_header, 'DATAMIN',  datminmax[0], 'MINIMUM DATA VALUE'
    sxaddpar, out_header, 'DATAMAX',  datminmax[1], 'MAXIMUM DATA VALUE'
  endelse

  return, out_header
end
