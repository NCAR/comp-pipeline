; docformat = 'rst'

;+
; From the equation of a circle:
;
; $$r^2 - 2 h r \cos(\theta - \alpha) + h^2 - R^2 = 0$$
;
; where `h` is the radius of the circle center, `alpha` is the angle of the
; center point, `R` is the radius of the circle, and `theta` and `r` are the
; polar coordinates of the circle.
;
; Solve for as a function of the other variables via the quadratic equation:
;
; $$r = h \cos(\theta - \alpha) \pm \sqrt{h^2 cos^2(\theta - \alpha) - h^2 + R^2}$$
;
; define: `p[0]=h`, `p[1]=alpha`, `p[2]=R`, `x=theta` (radians) and `y=r`
; (pixels).
;
; Want to minimize the residual in the radius measurements.
;
; Note: this routine assumes the radius of the circle is much larger than the
; radius of the center coordinate.
;
; :Returns:
;   $\chi^2$ as float
;
; :Params:
;   p : in, required, type=fltarr
;     parameters of fit
;-
function comp_circ_func, p
  @comp_fitc_common

  r = p[0] * cos(x - p[1]) + sqrt(p[0]^2 * cos(x - p[1])^2 - p[0]^2 + p[2]^2)

  chisq = total((r - y)^2)
  return, chisq
end
