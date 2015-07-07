; docformat = 'rst'

;+
; Look up occulter size (mm) from occulter ID number.
;
; :Returns:
;   float
;
; :Params:
;    occulter_id
;
; :Author:
;   sitongia
;-
function comp_occulter_id, occulter_id
  compile_opt strictarr

  sizes = [ 0.000, 20.599, 20.625, 20.676, 20.752, 20.803, 20.828, 20.879, $
           20.904, 20.955, 20.955, 21.006, 21.031, 21.057, 21.082, 21.158, $
           21.209, 21.234, 21.285, 21.336, 21.387, 21.438, 21.463, 21.488, $
           21.615, 21.692, 21.692, 21.717, 21.793, 21.869, 21.946, 22.022, $
           22.073, 22.123, 22.174, 22.225]

  return, sizes[occulter_id]
end