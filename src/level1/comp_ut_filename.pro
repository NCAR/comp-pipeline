; docformat = 'rst'

;+
; The CoMP image files are named using Hawaiian Standard Time (HST). This
; function converts the timestamp portion of the input filename to UTC and
; returns the new root.
;
; :Params:
;   hst_filename : in, required, type=string
;     input filename in HST format
;
; :Author:
;   sitongia
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

  jd = julday(month, day, year, hour, minute, second)

  ; add 10 hours get UT
  jd += 10.D / 24.D

  caldat, jd, ut_month, ut_day, ut_year, ut_hour, ut_minute, ut_second

  rsecond = round(ut_second)
  if (rsecond gt 59) then begin
    ut_second -= 60
    ut_minute += 1
  endif

  if (ut_minute gt 59) then begin
    ut_minute -= 60
    ut_hour += 1
  endif

  if (ut_hour gt 23) then begin
    ut_hour -= 24
    ut_day += 1
  endif

  if (ut_day gt 31) then begin
    ut_day -= 31
    ut_month += 1
  endif

  if (ut_month gt 12) then begin
    ut_month -= 12
    ut_year += 1
  endif

  ut_filename = string(ut_year, ut_month, ut_day, $
                       ut_hour, ut_minute, ut_second, $
                       format='(%"%04d%02d%02d.%02d%02d%02d")')

  return, ut_filename
end
  