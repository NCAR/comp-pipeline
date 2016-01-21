; docformat = 'rst'

;+
; Produce the standard set of spatial basis functions for the inversion - in this case,
; bilinear in x and y. Optional keyword arguments:
;	nx: Number of x pixels (default 1024). Ignored if xmat and ymat are given as inputs.
;	ny: Number of y pixels (default 1024). Ignored if xmat and ymat are given as inputs.
;	xmat(nx,ny): Array of x pixel coordinates. Can be used as input, or array will be created and returned.
;	ymat(nx,ny): Array of y pixel coordinates. Can be used as input, or array will be created and returned.
;
; Returns the array of spatial basis images (dimensions [nx,ny,nbasis]).
;
; Joseph Plowman
;-
function comp_cal_xybasis, nx=nx, ny=ny, xmat=xmat, ymat=ymat

	; See if we need to create the x and y pixel coordinate matrices:
	if(n_elements(xmat) eq 0 or n_elements(ymat) eq 0) then begin
		if(n_elements(nx) eq 0) then nx=1024
		if(n_elements(ny) eq 0) then ny=1024
		xmat=(dindgen(nx))#transpose(1.0+dblarr(ny))-0.5*(nx-1.0)
		ymat=(1.0+dblarr(nx))#transpose(dindgen(ny))-0.5*(ny-1.0)
	endif else begin
		nx=n_elements(xmat[*,0])
		ny=n_elements(xmat[0,*])
	endelse

	; Create the array for the basis and assign its elements:
	xybasis = fltarr(nx,ny,4)
	xybasis[*,*,0]=1.0 ; Constant term.
	xybasis[*,*,1]=xmat ; Varying only with x coordinate.
	xybasis[*,*,2]=ymat ; Varying with y coordinate.
	xybasis[*,*,3]=xmat*ymat ; Bilinear in x and y.

	return, xybasis ; Done.

end
