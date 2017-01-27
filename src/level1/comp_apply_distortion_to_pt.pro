; docformat = 'rst'

;+
; Apply to the distortion to a point described in pixel coordinates.
;
; :Returns:
;   fltarr(2)
;
; :Params:
;   xy : in, required, type=fltarr(2)
;     a point in pixel space to distort, coordinates should 0..619 for
;   dx_c : in, required, type="fltarr(3, 3)"
;     x coefficients for distortion
;   dy_c : in, required, type="fltarr(3, 3)"
;     y coefficients for distortion
;-
function comp_apply_distortion_to_pt, xy, dx_c, dy_c
  compile_opt strictarr

  new_x = xy[0] + comp_eval_surf(dx_c, xy[0], xy[1])
  new_y = xy[1] + comp_eval_surf(dy_c, xy[0], xy[1])
  return, [new_x, new_y]
end
