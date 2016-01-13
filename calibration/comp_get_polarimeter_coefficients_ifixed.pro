; docformat = 'rst'

;+
; Function which computes the coefficients of the 'crosstalk', or response of each
; pixel to the Stokes vector, for a single configuration of the polarization analyzer
; (or linear combination thereof, such as I+Q-(I-Q)), given a set of data and variances
; with known input Stokes vectors (as produced by the CoMP calibration optics with
; given configuration parameters) - there must be at least 4 linearly independent
; Stokes vectors (and corresponding data) for a well-constrained solution to exist.
; Inputs:
; 	data[npix,nstates] - The calibration data for one state of the polarization analyzer,
;			one image (previously flattened using where(mask)) for each calibration optics
;			configuration measured.
; 	vars[npix,nstates] - The variances corresponding to data.
; 	pols[nstates,nstokes] - The input calibration Stokes vectors for each calibration optics
;			configuration.
; 	xybasis[npix,nbasis] - Basis specifying the varying sensitivity of each pixel; created
;			from comp_cal_xybasis, but flattened using where(mask).
; Returns a set of coefficients defining the spatially varying response from each
; component of the Stokes vector into the detected signal for this state
; of the polarization analyzer (or linear combination of states). There are
; nstokes*n_basis (typically 16 total) of these, varying first over spatial
; basis coefficients, then over Stokes vector component (i.e., elements 0...3
; define the spatially varying sensitivity to Stokes I, 4...7 is Stokes Q, etc).
;
; This version operates on flattened images which have had masked pixels
; already removed. The other version, get_polarimeter_coefficients_slow, operates
; on a set of ordinary images, and is easier to understand but slower; The
; two versions should produce identical results.
;
; Joseph Plowman
;-
function comp_get_polarimeter_coefficients_ifixed, data, vars, pols, xybasis, c0=c0

	nstates = n_elements(data[0,*])
	nstokes = n_elements(pols[0,*])-1
	if(n_elements(c0) eq 0) then c0 = 1.0

	nc_spatial = n_elements(xybasis[0,*])
	n_coef = nstokes*nc_spatial

	; Problem to be solved is of the form A*x=b, where x is the solution vector of coefficients
	; determining the crosstalk into this channel. The solution vector collapses crosstalk
	; and spatial variation onto a single index; jlvec reverses this mapping:
	jlvec = lonarr(n_coef,2) ; This array gives the mapping from n_coef to nstokes*nc_spatial
	for j=0,nstokes-1 do for l=0,nc_spatial-1 do jlvec[j*nstokes+l,*]=[j,l]

	bvec = dblarr(n_coef)
	amat = dblarr(n_coef,n_coef)
	for i=0,n_coef-1 do begin
		j=jlvec[i,0]
		l=jlvec[i,1]
		for k=0,nstates-1 do begin
			bvec[i]+=total((data[*,k]-c0*pols[k,0])*xybasis[*,l]/vars[*,k])*pols[k,j]
			for i2=0,n_coef-1 do begin
				j2=jlvec[i2,0]
				l2=jlvec[i2,1]
				amat[i,i2]+=total(xybasis[*,l]*xybasis[*,l2]/vars[*,k])*pols[k,j+1]*pols[k,j2+1]
			endfor
		endfor
	endfor

	return, [c0,reform(invert(amat,/double)#bvec)]

end
