; docformat = 'rst'

;+
; Function to return sinc function
;-
function sinc, x
  compile_opt strictarr

  f = sin(!pi * x) / (!pi * x)
  bad = where(x eq 0.0, count)
  if (count gt 0) then f[bad] = 1.0

  return, f
end
