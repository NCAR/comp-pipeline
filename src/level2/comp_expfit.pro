; docformat = 'rst'

;+
; Evaluate exponential function. `p` is the form $$y = a e^(-(x - 1) / b)$$
;
; Note: `r` is in solar radii.
;
; :Returns:
;
; :Params:
;   r : in, required, type=fltarr
;     solar radii
;   p : in, required, type=fltarr(2)
;     parameters
;-
function comp_expfit, r, p
  compile_opt strictarr

  return, p[0] * exp(-(r - 1) / p[1])
end
