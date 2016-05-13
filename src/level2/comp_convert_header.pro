; docformat = 'rst'

;+
; Convert a level 1 header to a level 2 header.
;
; :Uses:
;   sxaddpar, sxdelpar
;
; :Params:
;   in_header : in, required, type=strarr
;     FITS header
;
; :Keywords:
;   exten : in, optional, type=integer
;     extension number; if not present, assume it is primary header
;   extname : in, optional, type=string
;     extension name; must be present if exten is present and more than 1
;   datminmax : in, optional, type=fltarr(2)
;     min/max of data; must be present if exten is present and more than 1
;
; :History:
;   written by Christian Bethge
;   modified by mdg 5/13/2016
;-
function comp_convert_header, in_header, $
                              exten=exten, $
                              extname=extname, $
                              datminmax=datminmax
  compile_opt strictarr

  out_header = in_header

  ; if EXTEN is present and 1 or more
  if (keyword_set(exten)) then begin
    rtags = ['POLSTATE', 'NAVERAGE', 'FILTER', 'PCOUNT', 'GCOUNT', 'DATATYPE', $
             'BODYTEMP', 'BASETEMP', 'RACKTEMP', 'EXPOSURE', 'OPTRTEMP', $
             'DEMULT', 'FILTTEMP', 'FITMNLIN', 'FITVRLIN', 'FLATFILE']

    sxdelpar, out_header, rtags

    sxaddpar, out_header, 'XTENSION', 'IMAGE', 'extension type', before='BITPIX'
    sxaddpar, out_header, 'EXTNAME',  extname, before='BITPIX'
    sxaddpar, out_header, 'DATAMIN',  datminmax[0], 'MINIMUM DATA VALUE'
    sxaddpar, out_header, 'DATAMAX',  datminmax[1], 'MAXIMUM DATA VALUE'
  endif else begin
    rtags = ['NTUNES', 'TNELNGTH', 'TUNEDLAY', 'H_D$OCCULT', 'V_D$OCCULT', $
             'FOCUS', 'COVER', 'POLANGLE', 'POLARIZR', 'OPAL', 'RETARDER', $
             'OXCNTER1', 'OYCNTER1', 'OXCNTER2', 'OYCNTER2', 'FXCNTER1', $
             'FYCNTER1', 'FXCNTER2', 'FYCNTER2', 'METHOD', 'OCRAD1', $
             'OCRAD2', 'FCRAD1', 'FCENX1', 'FCENY1', 'FCRAD2', 'FCENX2', $
             'FCENY2', 'CRRADIUS', 'OCC_D$ID', 'OCC_D$SIZE']

    sxdelpar, out_header, rtags

    time_obs = sxpar(in_header, 'DATE_HST') + ' ' + sxpar(in_header, 'TIME_HST')
    sxaddpar, out_header, 'DATE_HST', time_obs

    sxaddpar, out_header, 'LEVEL',  'L2', ' '
    sxaddpar, out_header, 'EXTEND', 'T', 'file may contain extensions', $
              after='NAXIS'
  endelse

  return, out_header
end
