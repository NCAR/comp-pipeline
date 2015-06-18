; docformat = 'rst'

;+
; Function returning the time of day in correct format for `READ_FLATS`, among
; other things.
;
; :Uses:
;   sxpar
;
; :Returns:
;   time of day in units of hours as a float
;
; Inputs:
;   headers : in, required, type="strarr(ntags, nimages)"
;     header array from which to take date and time (only first header in array
;     is used)
;   day : out, required, type=integer
;     the day of the month.
;   month : out, required, type=integer
;     the month
;   year : out, required, type=integer
;     the year
;   hours : out, required, type=integer
;     hour of day
;   mins : out, required, type=integer
;     minute of hour
;   secs : out, optional, type=integer
;     second of minute
;
; :Author:
;   Joseph Plowman
;-
function comp_extract_time, headers, day, month, year, hours, mins, secs
  compile_opt strictarr

  date_str = sxpar(headers[*, 0], 'DATE_OBS')
  time_str = sxpar(headers[*, 0], 'TIME_OBS')

  mdy = strsplit(date_str, '/', /extract)
  month = fix(mdy[0])
  day = fix(mdy[1])
  year = fix(mdy[2])

  if (year lt 2000) then year += 2000

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

  return, float(hours) + float(mins) / 60.0 + float(secs) / 3600.0
end

