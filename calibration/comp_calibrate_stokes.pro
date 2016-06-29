;+
; Compute demodulated stokes vector from a set of flat and dark corrected Stokes images.
;
; Inputs:
;	coef_images[nx,ny,nstates,nstokes]: Images of the calibration coefficients.
;	data[nx,ny,nstates]: Data images, one for each observed polarimeter state. Ordering of nstates
;						in data and vars must match ordering in coef_images.
;	vars[nx,ny,nstates]: Variances corresponding to data.
;
; Returns a set of images for the demodulated Stokes components [nx,ny,n_stokes].
;
; Joseph Plowman
;-
function comp_compute_calibrated_stokes, coef_images, data, vars

	nx = n_elements(data[*,0,0])
	ny = n_elements(data[0,*,0])
	nstates = n_elements(data[0,0,*])
	nstokes = n_elements(coef_images[0,0,0,*])
	amat = dblarr(nstokes,nstokes)
	bvec = dblarr(nstokes)
	stokes_images = dblarr(nx,ny,nstokes)

	for x=0,nx-1 do begin
		for y=0,ny-1 do begin
			for j=0,nstokes-1 do begin
				for k=0,nstokes-1 do begin
				    ; equation 30 from writeup
					amat[j,k] = total(coef_images[x,y,*,j]*coef_images[x,y,*,k]/vars[x,y,*])
				endfor
				; equation 31 from writeup
				bvec[j] = total(data[x,y,*]*coef_images[x,y,*,j]/vars[x,y,*])
			endfor
			; equation 32 from writeup
			stokes_images[x,y,*] = invert(amat,/double)#bvec
		endfor
	endfor

	return,stokes_images

end

;+
; Compute calibration coefficient images, as used by comp_compute_calibrated_stokes.
; This assumes we want the full 4-dimensional Stokes vector - modifying to do the
; reduced rank inversion is left as an exercise...
;
; Inputs:
;	cal_struct: Structure containing calibration information, as produced by
; 				comp_cal_example_script.pro
;	data_labels: Array of labels of the polarization states in the data array which is
;				to be calibrated.
;
; Returns an array of calibration coefficients which can be used to calibrate
; and demodulate the data array. Note that the array will have the same
; dimensions as the arrays in the calibration structure.
;
; Joseph Plowman
;-
function comp_compute_coef_images, cal_struct, data_labels

	nstokes=4
	nx = n_elements(cal_struct.mask[*,0])
	ny = n_elements(cal_struct.mask[0,*])
	n_basis = n_elements(cal_struct.xybasis[0,0,*])
	nlabels = n_elements(data_labels)
	coef_images = dblarr(nx,ny,nlabels,nstokes)

	for i=0,nlabels-1 do begin
		ical = where(data_labels[i] eq cal_struct.upols)
		for j=0,nstokes-1 do begin
			for k=0,n_basis-1 do begin
				upper = cal_struct.uppercoefs[ical,j*n_basis+k]*cal_struct.uppermask
				lower = cal_struct.lowercoefs[ical,j*n_basis+k]*cal_struct.lowermask
				coef_images[*,*,i,j] += (upper+lower)*cal_struct.mask*cal_struct.xybasis[*,*,k]
			endfor
		endfor
	endfor

	return,coef_images

end

;+
; Average, demodulate, and calibrate an input array of data and variances
; using the calibration information information in an input calibration
; structure.
;
; Inputs:
;	data[nx,ny,nstates]: Data images, one for each observed polarimeter state.
;	vars[nx,ny,nstates]: Variances corresponding to data.
;	labels[nstates]: Strings labeling each image in the data. must have same
;			name format as cal_struct.upols
;	cal_struct: Structure containing calibration information, as produced by
; 				comp_cal_example_script.pro
;
; Optional output keyword: stokeslabels - 4 element string containing the labels of
;			the inverted Stokes images ('I','Q','U', and/or 'V').
;
; Returns the [nx,ny,nstokes] image array of the inverted Stokes vector.
;
; Joseph Plowman
;-
function comp_calibrate_stokes, data, vars, labels, cal_struct, stokeslabels=stokeslabels

	stokeslabels=['I','Q','U','V']
	coef_images = comp_compute_coef_images(cal_struct, labels)
	return, comp_compute_calibrated_stokes(coef_images, data, vars)

end