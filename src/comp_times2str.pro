; docformat = 'rst'

;+
; Create nicely formatted strings for a given decimal hour time. Could give a
; time at 24:00:00 or later, meaning the next day.
;
; :Returns:
;   string/strarr
;
; :Params:
;   times : in, required, type=fltarr
;     decimal times in 0.0 to 24.0
;-
function comp_times2str, times
  compile_opt strictarr

  _times = double(times)

  hours = floor(_times)
  minutes = floor(60 * (_times - hours))
  seconds = round(60 * 60 * (_times - hours - minutes / 60.0D))

  hours = string(hours, format='(%"%02d")')
  minutes = string(minutes, format='(%"%02d")')
  seconds = string(seconds, format='(%"%02d")')

  if (seconds ge 60L) then begin
    seconds -= 60L
    minutes += 1L
  endif
  if (minutes ge 60L) then begin
    minutes -= 60L
    hours += 1L
  endif

  return, hours + ':' + minutes + ':' + seconds
end
