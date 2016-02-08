; docformat = 'rst'

;+
; Function which computes the Mueller matrix of a retarder given its
; transmission, retardation, and orientation angles.
;
; :Returns:
;   the 4x4 Mueller matrix of the retarder
;
; :Params:
;  trans : in, required
;    the transmission
;  angle : in, required, type=double
;    the orientation angle, in degrees
;  delta : in, required, type=double
;    the retardation, in degrees
;
; :Author:
;   Joseph Plowman
;-
function comp_mueller_retarder, trans, angle, delta, incidence=incidence
  compile_opt strictarr

  ang = angle * !dpi / 180.0D
  del = delta * !dpi / 180.0D

  if (keyword_set(incidence)) then del *= 1.0D + (sin(incidence * !dpi / 180.0D)^2)/5.0D

  c2 = cos(2.0D * ang)
  s2 = sin(2.0D * ang)

  matrix = trans * [[1.0D, 0.0D, 0.0D, 0.d0], $
                    [0.0D, c2^2 + s2^2 * cos(del), c2 * s2 * (1. - cos(del)), -s2 * sin(del)], $
                    [0.0D, c2 * s2 * (1. - cos(del)), s2^2 + c2^2 * cos(del), c2 * sin(del)], $
                    [0.0D, s2 * sin(del), -c2 * sin(del), cos(del)]]
  return, matrix
end
