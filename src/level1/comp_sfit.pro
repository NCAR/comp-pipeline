; docformat = 'rst'

;+
; This function determines a polynomial fit to a surface sampled over a regular
; or irregular grid.
;
; Fit a 2D array Z as a polynomial function of x and y. The function fitted is:
;
; $$F(x,y) = \sum_{i,j} kx[j,i] x^i y^j$$
;
; :Categories:
;   curve and surface fitting
;
; :Returns:
;   the array of coefficients for a polynomial function of x and y to fit data
;   as a `Degree + 1` by `Degree + 1` element array
;
; :Params:
;   z : in, required, type="fltarr(3, n) or fltarr(m, n)"
;     the array of data to fit; if `IRREGULAR` is set, a `[3, n]` array
;     containing the X, Y, and Z location of each point sampled on the surface,
;     otherwise a `[m, n]` array
;   degree : in, required, type=integer
;     the maximum degree of fit (in one dimension)
;
; :Keywords:
;   irregular : in, optional, type=boolean
;     if set, `z` is [3, n] array, containing the X, Y, and Z locations of n
;     points sampled on the surface
;   max_degree : in, optional, type=boolean
;     if set, the `degree` parameter represents the maximum degree of the
;     fitting polynomial of all dimensions combined, rather than the maximum
;     degree of the polynomial in a single variable. For example, if `degree`
;     is 2, and `MAX_DEGREE` is not set, then the terms returned will be::
;
;       [[K, y, y^2], [x, xy, xy^2], [x^2, x^2 y, x^2 y^2]]
;
;     If `MAX_DEGREE` is set, the terms returned will be in a vector::
;
;       [K, y, y^2, x, xy, x^2]
;
;     in which no term has a power higher than two in X and Y combined, and
;     the powers of Y vary the fastest.
;
; :Author:
;   MLSO Software Team
;
; :History:
;   July, 1993, DMS  Initial creation
;   July, 2001  Added `MAX_DEGREE` and `IRREGULAR` keywords.
;   see git log for recent changes
;
; :Copyright:
;   Copyright (c) 1993-2013, Exelis Visual Information Solutions, Inc. All
;   rights reserved. Unauthorized reproduction is prohibited.
;-
function comp_sfit, z, degree, irregular=irregular, max_degree=max_degree
  compile_opt strictarr
  ;on_error, 2

  n_dims = size(z, /n_dimensions)
  dims = size(z, /dimensions)

  ; # of coeff to solve
  n_coeff = keyword_set(max_degree) ? (degree + 1L) * (degree + 2L): (degree + 1)^2

  if (keyword_set(irregular)) then begin
    if (n_dims ne 2) or (dims[0] ne 3) then begin
      message, 'for IRREGULAR grids, input must be [3, n]'
    endif

    ; # of points to fit
    n_pts  = dims[1]

    ; do it in double precision
    x  = double(z[0, *])
    y  = double(z[1, *])
    zz = double(z[2, *])
  endif else begin
     if (n_dims ne 2L) then message, 'for regular grids, input must be [nx, ny]'
     nx = dims[0]
     ny = dims[1]

     ; # of points to fit
     n_pts = nx * ny

     ; X and Y values at each point
     x = findgen(nx) # replicate(1.0, ny)
     y = replicate(1.0, nx) # findgen(ny)
  endelse

  if (n_coeff gt n_pts) then begin
    message, string(degree, n_coeff, $
                    format='(%"fitting degree of %d requires %d points")')
  endif

  ut = dblarr(n_coeff, n_pts, /nozero)

  ; NOTE: this nested loop is only executed 9 times when degree=2, so
  ; performance is not an issue
  k = 0L
  for i = 0L, degree do begin
    for j = 0L, degree do begin   ; fill each column of basis
      if (keyword_set(max_degree) and (i + j gt degree)) then continue
      ut[k, 0] = reform(x^i * y^j, 1, n_pts)
      k += 1
    endfor
  endfor

  kk = invert(ut # transpose(ut)) # ut

  ; coefficients
  kx = float(kk # reform(keyword_set(irregular) ? zz : z, n_pts, 1))

  if (~keyword_set(max_degree)) then kx = reform(kx, degree + 1L, degree + 1L)

  return, kx
end
