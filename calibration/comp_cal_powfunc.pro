; docformat = 'rst'

;+
; Function to fit crosstalk coefficients and evaluate chi squared given an set of input
; calibration optics information, which may include input stokes vector, polarizer angle error, retardance,
; polarizer and retarder transmission, and retarder angle, although not all of these may be uniquely
; defined using the data and this model. At a minimum, should search over the retardance and retarder angle.
; at present, the contents of the input vector are manually specified here and in the calling routine,
; and the remaining components are hardwired here. This should probably be moved to the common block
; or something slightly less kludgy.
;
; 	Optional parameter: diag_plot_dir - specifies a directory for diagnostic plots of the data and fit
;			(see `comp_plot_cal_comblk_data`).
;
;	Returns a fit chi squared, which will in principle be of order unity if the model is a good fit and
;			the variances contained in the common block (vars) are correct.
;
; Joseph Plowman
;-
function comp_cal_powfunc, input, diag_plot_dir=diag_plot_dir

	common comp_cal_comblk, xybasis, xyb_upper, xyb_lower, dataupper, datalower, varsupper, varslower, $
			xmat, ymat, cpols, pangs, crets, upols, datapols, datacals, cal_data, uppercoefs, $
			lowercoefs, uppermask, lowermask, data, vars, mask, nstokes, ucals, calvars, calvar_solve

	nupols = n_elements(upols)
	nbasis = n_elements(xybasis[0,0,*])
	ndata = n_elements(data[0,0,*])
	cal_data = 0.0*data

	; Arrays which will hold the coefficients for the upper and lower (above and below the diagonal)
	; fits to the crosstalk:
	uppercoefs = dblarr(nupols,nstokes*nbasis)
	lowercoefs = dblarr(nupols,nstokes*nbasis)

	; Update the non-fixed calibration variables with the most recent input:
	calvars[calvar_solve] = input

	; Set up the variables for the calibration optics:
	stokesin = calvars[0:3] ; The input Stokes vector.
	ptrans = calvars[4] ; Calibration polarizer transmission.
	pangerr = calvars[5] ; Systematic offset error in the polarizer angle (in degrees).
	rtrans = calvars[6] ; Calibration retarder transmission.
	ret = calvars[7] ; Calibration retarder retardance (in degrees).
	r_ang = calvars[8] ; Calibration retarder angle (in degrees).

	; Chi squared penalty on transmissions greater than 1:
	penalty = exp(((rtrans-1 > 0)+(ptrans-1 > 0))/.01)

	; Loop over unique polarization analyzer states (nominal 'I+Q', 'I+U', etc):
	for i=0,nupols-1 do begin
		; Find which calibration optics configurations for which we have data in
		; the current polarization analyzer state:
		icurrent = where(datapols eq upols[i])
		ncurrent = n_elements(icurrent)

		; Create the Stokes vectors for each calibration optics configuration observed by
		; the current polarization analyzer state:
		pols = dblarr(ncurrent,nstokes)
		for j=0,ncurrent-1 do begin
			pols[j,*] = get_cal_stokes_vector(crets[icurrent[j]], pangs[icurrent[j]]+pangerr, ret=ret,$
					r_ang=r_ang, stokes=stokesin, ptrans=ptrans, rtrans=rtrans, cpol=cpols[icurrent[j]])
		endfor

		; Linear inversion to compute the upper and lower crosstalk coefficients into the polarization
		; analyzer states:
		uppercoefs[i,*]=comp_get_polarimeter_coefficients(dataupper[*,icurrent],varsupper[*,icurrent],pols, $
				xyb_upper)
		lowercoefs[i,*]=comp_get_polarimeter_coefficients(datalower[*,icurrent],varslower[*,icurrent],pols, $
				xyb_lower)

		; Compute the calibration data corresponding to these coefficients:
		coefscurrent={uppercoefs:uppercoefs[i,*], lowercoefs:lowercoefs[i,*], xmat:xmat, ymat:ymat, $
				mask:mask, uppermask:uppermask, lowermask:lowermask, pols:pols, xybasis:xybasis}
		for j=0,ncurrent-1 do cal_data[*,*,icurrent[j]] = compute_cal_image(coefscurrent,pols[j,*])
	endfor

	; Compute chi squared between cal_data and data:
	chi2=0.0 ; Initialize chi squared.
	for i=0,ndata-1 do begin
		resids = reform((cal_data[*,*,i]-data[*,*,i])^2.0/vars[*,*,i])
		chi2 += total(mask*resids,/nan)
	endfor
	chi2 /= total(mask)*ndata

	; Plot diagnostic data if the plot directory parameter is set:
	if(n_elements(diag_plot_dir) gt 0) then comp_plot_cal_comblk_data, diag_plot_dir

	print,chi2*penalty,input ; Printout to show progress of inversion.
	return,chi2*penalty ; Done.

end
