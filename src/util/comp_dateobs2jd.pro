; docformat = 'rst'

function comp_dateobs2jd, dateobs
  compile_opt strictarr

  year   = long(strmid(dateobs, 0, 4))
  month  = long(strmid(dateobs, 5, 2))
  day    = long(strmid(dateobs, 8, 2))
  hour   = long(strmid(dateobs, 11, 2))
  minute = long(strmid(dateobs, 14, 2))
  second = long(strmid(dateobs, 17, 2))

  return, julday(month, day, year, hour, minute, second)
end
