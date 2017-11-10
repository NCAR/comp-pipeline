; docformat = 'rst'

;+
; Procedure to iteractively fit the coordinates of a circle in polar
; coordinates.
;
; The values of the fit are returned in a three element vector in the order:
; radius of the circle center, angle of the circle center, radius of the
; circle.
;
; :Examples:
;   For example, call like::
;
;     a = comp_circfit(theta, r)
;
; :Uses:
;   comp_fitc_common
;
; :Returns:
;   circle parameters in the form of `fltarr(3) = [h, alpha, R]` where `h` is
;   the radius of the circle center, `alpha` is the angle of the center point,
;   `R` is the radius of the circle
;
; :Params:
;   theta
;     the angle coordinates
;   r
;     the radius coordinates
;
; :Keywords:
;   chisq : out, optional, type=float
;     the optional value of the chisq
;   error : out, optional, type=long
;     0 if no error
;
; :Author:
;   MLSO Software Team
;-
function comp_circfit, theta, r, chisq=chisq, error=error
  compile_opt strictarr
  @comp_fitc_common

  debug = 0B
  error = 0L

  x = theta
  y = r
  a = [0.0, 0.0, mean(y)]
  count = 1

  while (count gt 0) do begin
    a = amoeba(1.e-4, p0=a, function_name='comp_circ_func', scale=2.0, $
               function_value=fval, nmax=10000)
    mg_log, 'chi^2: %0.3f', fval[0], name='comp', /debug

    ; check if amoeba failed: it returns -1 but usually returns an array, so
    ; use following hack rather than directly checking return value!
    if (size(a, /n_dimensions) eq 0) then begin
      error = 1L
      mg_log, 'AMOEBA failed', name='comp', /debug
      return, -1
    endif

    rfit = a[0] * cos(x - a[1]) $
             + sqrt(a[0]^2 * cos(x - a[1])^2 - a[0]^2 + a[2]^2)

    diff = y - rfit
    rms = stdev(diff)
    bad = where(abs(diff) ge 3. * rms, count, complement=good)

    if (count gt 0) then begin
      x = x[good]
      y = y[good]
      mg_log, '  %d bad point%s', $
              count, $
              count gt 1 ? 's' : '', $
              name='comp', /debug
    endif
  endwhile

  if (arg_present(chisq)) then chisq = total((r[good] - rfit[good])^2)

  return, a
end
