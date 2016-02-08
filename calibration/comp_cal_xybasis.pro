; docformat = 'rst'

;+
; Produce the standard set of spatial basis functions for the inversion, in
; this case, bilinear in x and y.
;
; :Returns:
;   the array of spatial basis images (dimensions `[nx, ny, nbasis]`)
;
; :Keywords:
;   nx : in, optional, type=integer, default=1024
;     number of x pixels; ignored if xmat and ymat are given as inputs.
;   ny : in, optional, type=integer, default=1024
;     number of Y pixels; ignored if xmat and ymat are given as inputs.
;  xmat : in, optional, type="dblrarr(nx, ny)"
;    array of x pixel coordinates; can be used as input, or array will be
;    created and returned
;  ymat : in, optional, type="dblrarr(nx, ny)"
;    array of y pixel coordinates; can be used as input, or array will be
;    created and returned
;
; :Author:
;   Joseph Plowman
;-
function comp_cal_xybasis, nx=nx, ny=ny, xmat=xmat, ymat=ymat
  compile_opt strictarr

  ; see if we need to create the x and y pixel coordinate matrices
  if (n_elements(xmat) eq 0 or n_elements(ymat) eq 0) then begin
    if (n_elements(nx) eq 0) then nx = 1024
    if (n_elements(ny) eq 0) then ny = 1024
    xmat = (dindgen(nx)) # transpose(1.0 + dblarr(ny)) - 0.5D * (nx - 1.0D)
    ymat = (1.0 + dblarr(nx)) # transpose(dindgen(ny)) - 0.5D * (ny - 1.0D)
  endif else begin
    nx = n_elements(xmat[*, 0])
    ny = n_elements(xmat[0, *])
  endelse

  ; create the array for the basis and assign its elements
  xybasis = fltarr(nx, ny, 4)
  xybasis[*, *, 0] = 1.0         ; constant term
  xybasis[*, *, 1] = xmat        ; varying only with x coordinate
  xybasis[*, *, 2] = ymat        ; varying with y coordinate
  xybasis[*, *, 3] = xmat * ymat ; bilinear in x and y

  return, xybasis
end
