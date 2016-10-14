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
;     p-angle
;
; :Keywords:
;   new_q : out, optional, type=fltarr
;     transformed Stokes Q
;   new_u : out, optional, type=fltarr
;     transformed Stokes U
;-
pro comp_polarimetric_transform, q, u, p_angle, new_q=new_q, new_u=new_u
  compile_opt strictarr

  new_q =   q * cos(2.0 * p_angle) + u * sin(2.0 * p_angle)
  new_u = - q * sin(2.0 * p_angle) + u * cos(2.0 * p_angle)
end
