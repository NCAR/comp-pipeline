function comp_hst2ut, year, month, day, hour, minute, second
  compile_opt strictarr

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

  return, {year: ut_year, $
           month: ut_month, $
           day: ut_day, $
           hour: ut_hour, $
           minute: ut_minute, $
           second: ut_second}
end
