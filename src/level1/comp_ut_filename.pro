; docformat = 'rst'

;+
; The CoMP image files are named using Hawaiian Standard Time (HST). This
; function converts the timestamp portion of the input filename to UTC and
; returns the new root.
;
; :Returns:
;   string
;
; :Params:
;   hst_filename : in, required, type=string
;     input filename in HST format
;
; :Author:
;   MLSO Software Team
;-
function comp_ut_filename, hst_filename
  compile_opt strictarr

  ; format is like 20101006.073518
  year   = long(strmid(hst_filename,  0, 4))
  month  = long(strmid(hst_filename,  4, 2))
  day    = long(strmid(hst_filename,  6, 2))
  hour   = long(strmid(hst_filename,  9, 2))
  minute = long(strmid(hst_filename, 11, 2))
  second = long(strmid(hst_filename, 13, 2))

  ut = comp_hst2ut(year, month, day, hour, minute, second)

  ut_filename = string(ut.year, ut.month, ut.day, $
                       ut.hour, ut.minute, ut.second, $
                       format='(%"%04d%02d%02d.%02d%02d%02d")')

  return, ut_filename
end
  