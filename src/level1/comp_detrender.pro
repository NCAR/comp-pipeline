; docformat = 'rst'

;+
; :Returns:
;   normalized fit
;
; :Params:
;   flat
;   mask
;-
function comp_detrender, flat, mask
  compile_opt strictarr

  s = size(flat)
  nx = s[1]

  ; inside of annulus
  good = where(mask eq 1.0)

  x = rebin(findgen(nx), nx, nx) - float(nx) / 2.
  y = transpose(x)

  xfit = x[good]
  yfit = y[good]
  zfit = flat[good]

  coefs = comp_surf_fit_xy(zfit, xfit, yfit, 2)
  fit = comp_eval_surf(coefs, reform(x[*, 0]), reform(y[0, *]))

  ; normalize fit
  fit /= mean(fit[good])
  return, fit
end
