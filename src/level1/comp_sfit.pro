; docformat = 'rst'

;+
; This function determines a polynomial fit to a surface sampled over a regular
; or irregular grid.
;
; Procedure:
;   Fit a 2D array Z as a polynomial function of x and y.
;
;   The function fitted is:
;       F(x,y) = Sum over i and j of kx[j,i] * x^i * y^j
;   where `kx` is returned as a keyword.
;
; :Categories:
;   curve and surface fitting
;
; :Returns:
;   the array of coefficients for a polynomial function of x and y to fit data
;   as a `Degree + 1` by `Degree+1` element array
;
; :Params:
;   z : in, required, type="fltarr(3, n)"
;     the array of data to fit, a `[3, n]` array containing the X, Y, and Z
;     location of each point sampled on the surface.
;   Degree : in, required, type=integer
;     the maximum degree of fit (in one dimension)
;
; :History:
;   July, 1993, DMS  Initial creation
;   July, 2001  Added MAX_DEGREE and IRREGULAR keywords.
;   March 2015, mdg, simplified for CoMP needs
;
; :Copyright:
;   Copyright (c) 1993-2013, Exelis Visual Information Solutions, Inc. All
;   rights reserved. Unauthorized reproduction is prohibited.
;-
function comp_sfit, z, degree
  compile_opt strictarr
  on_error, 2

  s = size(z)

  ; # of coeff to solve
  n2 = (degree + 1)^2

  if (s[0] ne 2) or (s[1] ne 3) then begin
    message, 'For IRREGULAR grids, input must be [3,n]'
  endif

  m  = n_elements(z) / 3    ; # of points
  x  = double(z[0, *])      ; do it in double...
  y  = double(z[1, *])
  zz = double(z[2, *])

  if (n2 gt m) then begin
    message, string(degree, n2, $
                    format='(%"Fitting degree of %d requires %d points")')
  endif

  ut = dblarr(n2, m, /nozero)
  k = 0L
  for i = 0L, degree do begin
    for j = 0L, degree do begin   ; fill each column of basis
      ut[k, 0] = reform(x^i * y^j, 1, m)
      ++k
    endfor
  endfor

  kk = invert(ut # transpose(ut)) # ut
  kx = float(kk # reform(zz, m, 1))   ; coefficients
  kx = reform(kx, degree + 1, degree + 1)

  return, kx
end
