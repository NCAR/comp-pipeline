; docformat = 'rst'

;+
; Perform a polarimetric transformation from geographic to heliographic
; coordinates on Q and U.
;
; Equation 6 from https://ned.ipac.caltech.edu/level5/Kosowsky/Kosowsky2.html
;
; :Params:
;   q : in, required, type=fltarr
;     Stokes Q
;   u : in, required, type=fltarr
;     Stokes U
;   p_angle : in, required, type=float
;     p-angle in degrees
;   overlap_angle : in, required, type=float
;     overlap angle in degrees
;
; :Keywords:
;   new_q : out, optional, type=fltarr
;     transformed Stokes Q
;   new_u : out, optional, type=fltarr
;     transformed Stokes U
;
; :Author:
;   MLSO Software Team
;-
pro comp_polarimetric_transform, q, u, p_angle, overlap_angle, $
                                 new_q=new_q, new_u=new_u
  compile_opt strictarr

  p_angle_radians = (p_angle - 45.0) * !dtor

  new_q =   q * cos(2.0 * p_angle_radians) + u * sin(2.0 * p_angle_radians)
  new_u = - q * sin(2.0 * p_angle_radians) + u * cos(2.0 * p_angle_radians)
end
