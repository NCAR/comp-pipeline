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
;   degree : in, required, type=integer
;     the maximum degree of fit (in one dimension)
;
; :Keywords:
;   irregular : in, optional, type=boolean
;     if set, `z` is [3, n] array, containing the X, Y, and Z locations of n
;     points sampled on the surface
;   max_degree : in, optional, type=boolean
;       if set, the `degree` parameter represents the maximum degree of the
;       fitting polynomial of all dimensions combined, rather than the maximum
;       degree of the polynomial in a single variable. For example, if `degree`
;       is 2, and `MAX_DEGREE` is not set, then the terms returned will be::
;
;         [[K, y, y^2], [x, xy, xy^2], [x^2, x^2 y, x^2 y^2]]
;
;       If `MAX_DEGREE` is set, the terms returned will be in a vector::
;
;         [K, y, y^2, x, xy, x^2]
;
;       in which no term has a power higher than two in X and Y combined, and
;       the powers of Y vary the fastest.
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
function comp_sfit, z, degree, irregular=irregular, max_degree=max_degree
  compile_opt strictarr
  on_error, 2

  s = size(z)

  ; # of coeff to solve
  n2 = keyword_set(max_degree) ? (degree + 1L) * (degree + 2L): (degree + 1)^2

  if (keyword_set(irregular)) then begin
    if (s[0] ne 2) or (s[1] ne 3) then begin
      message, 'For IRREGULAR grids, input must be [3, n]'
    endif

    m  = n_elements(z) / 3    ; # of points
    x  = double(z[0, *])      ; do it in double...
    y  = double(z[1, *])
    zz = double(z[2, *])
  endif else begin
     if (s[0] ne 2L) then message, 'For regular grids, input must be [nx, ny]'
     nx = s[1]
     ny = s[2]
     m = nx * ny                            ; # of points to fit
     x = findgen(nx) # replicate(1.0, ny)   ; X values at each point
     y = replicate(1.0, nx) # findgen(ny)
  endelse

  if (n2 gt m) then begin
    message, string(degree, n2, $
                    format='(%"Fitting degree of %d requires %d points")')
  endif

  ; TODO: see if this can be optimized
  ut = dblarr(n2, m, /nozero)
  k = 0L
  for i = 0L, degree do begin
    for j = 0L, degree do begin   ; fill each column of basis
      if (keyword_set(max_degree) and (i + j gt degree)) then continue
      ut[k, 0] = reform(x^i * y^j, 1, m)
      k += 1
    endfor
  endfor

  kk = invert(ut # transpose(ut)) # ut
  kx = float(kk # reform(zz, m, 1))   ; coefficients
  if (~keyword_set(max_degree)) then kx = reform(kx, degree + 1L, degree + 1L)

  return, kx
end
