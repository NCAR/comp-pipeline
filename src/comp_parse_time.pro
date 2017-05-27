; docformat = 'rst'

;+
; Parse a time string into numeric values.
;
; :Returns:
;   float representing time of day as decimal hours
;
; :Params:
;   time_str : in, required, type=string
;     time of day as a string, either H:MM:SS AM or HH:MM:SS AM
;
; :Keywords:
;   hours : out, required, type=integer
;     hour of day
;   minutes : out, required, type=integer
;     minute of hour
;   seconds : out, optional, type=integer
;     second of minute
;-
function comp_parse_time, time_str, hours=hours, minutes=mins, seconds=secs
  compile_opt strictarr

  if (strlen(time_str) eq 10) then begin
    hours = fix(strmid(time_str, 0, 1))
    mins = fix(strmid(time_str, 2, 2))
    secs = fix(strmid(time_str, 5, 2))
    time_label = strmid(time_str, 8, 2)
  endif else begin
    hours = fix(strmid(time_str, 0, 2))
    mins = fix(strmid(time_str, 3, 2))
    secs = fix(strmid(time_str, 6, 2))
    time_label = strmid(time_str, 9, 2)
  endelse

  if (strupcase(time_label) eq 'PM' and hours ne 12) then hours += 12.0
  if (strupcase(time_label) eq 'AM' and hours eq 12) then hours -= 12.0

  return, double(hours) + double(mins) / 60.0D + double(secs) / 3600.0D
end
