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
;     circfit, theta, r
;
; :Uses:
;   comp_fitc_common
;
; :Params:
;   theta
;     the angle coordinates
;   r
;     the radius coordinates
;
; :Keywords:
;   chisq
;     the optional value of the chisq
;
; :Author:
;   Tomczyk, modified by Sitongia
;-
function comp_circfit, theta, r, chisq=chisq
  compile_opt strictarr
  @comp_fitc_common

  ans = ' '
  debug = 0

  x = theta
  y = r
  a = [0., 0., mean(y)]
  count = 1

  while (count gt 0) do begin
    a = amoeba(1.e-4, p0=a, function_name='comp_circ_func', scale=60., $
               function_value=fval, nmax=10000)
    mg_log, 'chisq: %f', fval[0], name='comp/circfit', /debug

    ; check if amoeba failed: it returns -1 but usually returns an array, so
    ; use following hack rather than directly checking return value!
    s = size(a)
    if (s[0] eq 0) then begin
      message, 'circfit: amoeba failed.'
      break
    endif

    rfit = a[0] * cos(x - a[1]) $
             + sqrt(a[0]^2 * cos(x - a[1])^2 - a[0]^2 + a[2]^2)

    diff = y - rfit
    rms = stdev(diff)
    bad = where(abs(diff) ge 3. * rms, count, complement=good)
    if count gt 0 then begin
      x = x[good]
      y = y[good]
      mg_log, '%d bad points:', count, name='comp/circfit', /debug
      if (debug eq 1) then begin
        plot, x, y, title='circfit'
        oplot, x, rfit
        read, 'enter return', ans
      endif
    endif
  endwhile

  if (keyword_set(chisq)) then chisq = total((r[good] - rfit[good])^2)

  return,a
end
