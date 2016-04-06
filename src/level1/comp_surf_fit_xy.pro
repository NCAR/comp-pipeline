; docformat = 'rst'

;+
; Determine a polynomial fit to a surface.
;
; This function uses `POLYWARP` to determine the coefficients of the
; polynomial, then evaluates the polynomial to yield the fit surface.
;
; Generate coordinate arrays for `POLYWARP` using the indices as coordinates.
; The `yi` and `ky` arrays for `POLYWARP` are, in this usage, redundant, so
; they are sent as dummies. The coefficients returned from `POLYWARP` are then
; used in evaluating the polynomial fit to give the surface fit.
;
; The number of data points in data_surface must be greater or equal to
; `(degree + 1)^2`.
;
; :Categories:
;   E2 - Curve and surface fitting
;
; :Returns:
;   `SURFACE_FIT` returns a two-dimensional array of values from the evaluation
;   of the polynomial fit.
;
; :Params:
;   surf : in, required, type=2D numeric
;     The two-dimensional array of data to be fit to. The sizes of each
;      dimension may be unequal.
;   coord_x : in, required, type=1D numeric
;     x-coordinate of each point
;   coord_y : in, required, type=1D numeric
;     y-coordinate of each point
;   degree : in, required, type=long
;     maximum degree of fit (in one dimension)
;
; :History:
;   Written by:  Leonard Sitongia, LASP, University of Colorado,
;          April, 1984.
;   Modified by: Mike Jone, LASP, Sept. 1985.
;-
function comp_surf_fit_xy, surf, coord_x, coord_y, degree
  compile_opt strictarr
  on_error, 2

  sz = n_elements(surf)
  fit     = fltarr(sz)
  re_surf = fltarr(sz)

  ; compute fit coefficients
  polywarp, surf, re_surf, coord_x, coord_y, degree, coeff, re_coeff
  coeff = transpose(coeff)

  return, coeff
end
