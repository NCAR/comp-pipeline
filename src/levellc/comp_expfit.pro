; docformat = 'rst'

;+
; `parm` is the form $$y=a*e^(-(x - 1) / b)$$
;
; Note: `r` is in solar radii.
;
; :Returns:
;
; :Params:
;   r
;   parm
;-
function comp_expfit, r, parm
  compile_opt strictarr

  output = parm[0] * exp(-(r - 1) / parm[1])
  return, output
end
