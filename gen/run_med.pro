; docformat = 'rst'

;+
; Compute running median of array, using given window width.
;
; :Returns:
;   `fltarr(n)`
;
; :Params:
;   array : in, required, type=fltarr(n)
;     array to find running median of
;   width : in, required, type=integer
;     window widthx
;-
function run_med, array, width
  compile_opt strictarr

  n    = n_elements(array)
  rmed = fltarr(n)
  w2   = long(width / 2)

  for i = 0L, n - 1L do begin
    i1 = (i - w2) > 0
    i2 = (i + w2) < (n - 1)
    rmed[i] = median(array[i1:i2])
  endfor

  return, rmed
end
