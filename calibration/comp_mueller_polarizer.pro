; docformat = 'rst'

;+
; Function which computes the Mueller matrix of a linear polarizer given its
; transmission and orientation angle.
;
; :Returns:
;   the 4x4 mueller matrix
;
; :Params:
;   trans : in, required
;     the transmission
;   angle : in, required, type=double
;     the angle, in degrees
;
; :Author:
;   Joseph Plowman
;-
function comp_mueller_polarizer,trans,angle
  compile_opt strictarr

  ang = angle * !dpi /180.0D   ; convert to radians
  c2 = cos(2.0D * ang)
  s2 = sin(2.0D * ang)

  matrix = trans * [[1.0D, c2,    s2,    0.0D],$
                    [c2,   c2^2,  c2*s2, 0.0D],$
                    [s2,   s2*c2, s2^2,  0.0D],$
                    [0.0D, 0.0D,  0.0D,  0.0D]]

  return, matrix
end

