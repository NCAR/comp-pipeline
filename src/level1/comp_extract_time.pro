; docformat = 'rst'

;+
; Function returning the time of day in correct format for `COMP_READ_FLATS`.
;
; :Uses:
;   comp_parse_time, sxpar
;
; :Returns:
;   time of day in units of hours as a float
;
; :Params:
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
;   MLSO Software Team
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

  return, comp_parse_time(time_str, hours=hours, minutes=mins, seconds=secs)
end

