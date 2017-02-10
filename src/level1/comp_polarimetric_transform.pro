; docformat = 'rst'

;+
; Perform a polarimetric transformation from geographic to heliographic
; coordinates on Q and U.
;
; :Params:
;   q : in, required, type=fltarr
;     Stokes Q
;   u : in, required, type=fltarr
;     Stokes U
;   p_angle : in, required, type=float
;     p-angle in degrees
;
; :Keywords:
;   new_q : out, optional, type=fltarr
;     transformed Stokes Q
;   new_u : out, optional, type=fltarr
;     transformed Stokes U
;-
pro comp_polarimetric_transform, q, u, p_angle, new_q=new_q, new_u=new_u
  compile_opt strictarr

  ; TODO: we should add or subtract the overlap angle here
  p_angle_radians_q = (p_angle - 45.0) * !dtor
  p_angle_radians_u = (p_angle + 45.0) * !dtor

  new_q =   q * cos(2.0 * p_angle_radians_q) + u * sin(2.0 * p_angle_radians_q)
  new_u = - q * sin(2.0 * p_angle_radians_u) + u * cos(2.0 * p_angle_radians_u)
end
