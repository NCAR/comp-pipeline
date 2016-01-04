;+
; Makes plots of the crosstalk coefficients product by the calibration code, using information stored in the
; common block. Produces one plot file for each polarization analyzer state, containing plots of the
; to each component of the stokes vector (I, Q, U, and V). Takes as input the name of the directory
; to which the plots should be written.
;
; Joseph Plowman
;-
pro make_coef_plots, plot_directory
  common comp_cal_comblk, xybasis, xyb_upper, xyb_lower, dataupper, datalower, varsupper, varslower, $
      xmat, ymat, cpols, pangs, crets, upols, datapols, datacals, cal_data, uppercoefs, $
      lowercoefs, uppermask, lowermask, data, vars, mask, nstokes, ucals, calvars, calvar_solve
	
  ; Numbers of unique polarizations, x and y pixels, and spatial basis elements:
  nupols = n_elements(upols)
  nx = n_elements(xybasis[*,0,0])
  ny = n_elements(xybasis[0,*,0])
  n_basis = n_elements(xybasis[0,0,*])
	
  stokeslabels = ['I','Q','U','V'] ; The usual names for the Stokes vector components

  coef_image = dblarr(nx,ny) ; Array to hold the images

  ; Set up the plotting device:
  set_plot,'ps'
  device,color=1,bits_per_pixel=8,/inches,/encapsulated,xsize=10,ysize=10,decomposed=0
  !p.multi=[0,2,2]

  ; Color table which goes from purple to black to green:
  r=[0,reverse(dindgen(127)),dblarr(127),255]
  b=[0,dblarr(127),dindgen(127),255]
  tvlct,r,b,r
	
  if (~file_test(file_directory)) then file_mkdir, plot_directory
  ; Loop over unique polarizations (i.e., analyzer states; nominally I+Q, I+U, etc):
  for i=0,nupols-1 do begin
    ; Set the plot file name.
    device, filename=filepath('cal_coef_'+upols[i]+'.eps', root=plot_directory)
    for j=0,nstokes-1 do begin ; Loop over Stokes vector components.
      coef_image *= 0.0        ; Zero out the image to start.
      ; Loop over the basis elements, adding up the components of the image:
      for k=0,n_basis-1 do begin
        upperimage = mask*(uppercoefs[i,j*n_basis+k]*uppermask)*xybasis[*,*,k]
        lowerimage = mask*(lowercoefs[i,j*n_basis+k]*lowermask)*xybasis[*,*,k]
        coef_image += upperimage+lowerimage
      endfor
      ; Make the plot, with intensity range centered on zero:
      min_str=strtrim(string(min(coef_image[where(mask)])),2)
      max_str=strtrim(string(max(coef_image[where(mask)])),2)
      pmax = max(abs(coef_image[where(mask)]))
      plot_image,coef_image,title=stokeslabels[j]+', min='+min_str+', max=' $
                 +max_str,top=254,bottom=1,min=-pmax,max=pmax
    endfor
    device,/close
  endfor
end
		
	
;+
; Do a quick and dirty 'naive' demodulation (Q=I+Q-(I-Q), etc) using the contents of the 
; common block for plotting purposes. Inputs:
;	datain: Data array to plot - should have same size as common block data array.
;			handed in so that we can plot both the input data and calibration fit
;			data with the same routine and without a bunch of duplicated code
;			inside 'if' statements.
;	ucal: 	Which unique calibration optics state to do the demodulation for.
;	
; Outputs:
;	stokesi: Stokes I image.
;	stokesq: Stokes Q image.
;	stokesu: Stokes U image.
;	stokesv: Stokes V image.
;
; Joseph Plowman
;-
pro comp_cal_comblk_quickdemod, datain, ucal, stokesi, stokesq, stokesu, stokesv

	common comp_cal_comblk, xybasis, xyb_upper, xyb_lower, dataupper, datalower, varsupper, varslower, $
			xmat, ymat, cpols, pangs, crets, upols, datapols, datacals, cal_data, uppercoefs, $
			lowercoefs, uppermask, lowermask, data, vars, mask, nstokes, ucals, calvars, calvar_solve

	; Pull the parts we're interested in out of the input data array:	
	ipq = datain[*,*,where(datacals eq ucal and datapols eq 'I+Q')]
	imq = datain[*,*,where(datacals eq ucal and datapols eq 'I-Q')]
	ipu = datain[*,*,where(datacals eq ucal and datapols eq 'I+U')]
	imu = datain[*,*,where(datacals eq ucal and datapols eq 'I-U')]
	ipv = datain[*,*,where(datacals eq ucal and datapols eq 'I+V')]
	imv = datain[*,*,where(datacals eq ucal and datapols eq 'I-V')]
	
	; Demodulate them:
	stokesi = (ipq+imv+ipu+imu+ipv+imv)/6.0
	stokesq = (ipq-imq)/2.0
	stokesu = (ipu-imu)/2.0
	stokesv = (ipv-imv)/2.0

end	

;+
; Make plots of the data and fits (if run after fitting is complete) using the common block.
; Plots consist of one file for each calibration optics configuration, each showing 8 panels with
; the nominal I, Q, U, and V (i.e., what you'd get from naive demodulation of the nominal I+Q, I+U, 
; etc ignoring crosstalk), data on the left 4 and model fit on the right. File names and image titles
; have a key identifying the calibration optics configuration, which consists of a polarizer state 
; flag (1 = in, 0 = out), retarder state flag (1 = in, 0 = out), and polarizer angle, rounded to the 
; nearest degree. The only input is the directory to which the plots will be written.
;
; Joseph Plowman
;-
pro plot_cal_comblk_data, plot_dir

	common comp_cal_comblk, xybasis, xyb_upper, xyb_lower, dataupper, datalower, varsupper, varslower, $
			xmat, ymat, cpols, pangs, crets, upols, datapols, datacals, cal_data, uppercoefs, $
			lowercoefs, uppermask, lowermask, data, vars, mask, nstokes, ucals, calvars, calvar_solve
	
	ncals = n_elements(ucals)
	
	; Color table which goes from purple to black to green:
	r=[0,reverse(dindgen(127)),dblarr(127),255]
	b=[0,dblarr(127),dindgen(127),255]

	; Set up plotting device:
	if(n_elements(cal_data) eq n_elements(data)) then begin ; 8 panels if cal_data is present.
		!p.multi=[0,4,2]
		xsize=16
		ysize=8
	endif else begin ; Only plot 4 panels (the data) if cal_data is absent.
		!p.multi=[0,2,2]
		xsize=8
		ysize=8
	endelse			
	set_plot,'ps'
	device,color=1,bits_per_pixel=8,/inches,/encapsulated,xsize=xsize,ysize=ysize,decomposed=0
	
	; Loop over unique calibration optics configurations, and make plots for the nominal I, Q, U, and V
	; for the data and (if present) the calibration fit to the data. IDL multi-plot ordering goes left 
	; to right, then down to the next row, so we make the plots in the following order: I, Q, fitted I,
	; fitted Q, U, V, fitted U, fitted V. If there are no calibration fits, the order is just I, Q, U, V.
        if (~file_test(plot_dir)) then file_mkdir, plot_dir
	for i=0,ncals-1 do begin
		; File name for this optics configuration:
                filename = filepath('calplot_'+strjoin(strtrim(strsplit(ucals[i],' ',/extract),2))+'.eps', root=plot_dir)
		print,filename
		device,filename = filename

		; Quick and dirty 'naive' demodulation of the data:
		comp_cal_comblk_quickdemod,data, ucals[i], stokesi, stokesq, stokesu, stokesv		
		pmax = 2.0*median(stokesi[where(mask)]) ; Max intensity for Stokes I image.

		tvlct,findgen(256),findgen(256),findgen(256) ; Load black and white color table (for Stokes I).
		plot_image,stokesi*mask,min=0,max=pmax,title='I '+ucals[i] ; Plot stokes I.

		tvlct,r,b,r ; Load purple-green color table (for Stokes Q).
		plot_image,stokesq/stokesi*mask,min=-.3,max=.3,top=254,bottom=1,title='Q/I '+ucals[i] ; Plot Q.

		; If there is fitted calibration data (comp_cal_powfunc has been run), plot that too:
		if(n_elements(cal_data) eq n_elements(data)) then begin
			; Quick and dirty demodulation of the fitted calibration data:
			comp_cal_comblk_quickdemod,cal_data, ucals[i], stokesic, stokesqc, stokesuc, stokesvc
			pmaxc = 2.0*median(stokesic[where(mask)]) ; Max intensity level for fitted Stokes I.
			
			tvlct,findgen(256),findgen(256),findgen(256); Load black and white color table.
			plot_image,stokesic*mask,min=0,max=pmaxc,title='I (fit) '+ucals[i]; Plot fitted I.
			
			tvlct,r,b,r ; Load purple-green color table for fitted Stokes Q. Plot fit Q (below):
			plot_image,stokesqc/stokesic*mask,min=-.3,max=.3,top=254,bottom=1,title='Q/I (fit) '+ucals[i]
		endif
		; Plot Stokes U and V:
		plot_image,stokesu/stokesi*mask,min=-.3,max=.3,top=254,bottom=1,title='U/I '+ucals[i]
		plot_image,stokesv/stokesi*mask,min=-.3,max=.3,top=254,bottom=1,title='V/I '+ucals[i]
		; If fits are present, plot fit U and V:
		if(n_elements(cal_data) eq n_elements(data)) then begin
			plot_image,stokesuc/stokesic*mask,min=-.3,max=.3,top=254,bottom=1,title='U/I (fit)'+ucals[i]
			plot_image,stokesvc/stokesic*mask,min=-.3,max=.3,top=254,bottom=1,title='V/I (fit)'+ucals[i]
		endif
		device,/close
	endfor
	
end

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
;			(see plot_cal_comblk_data).
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
		uppercoefs[i,*]=get_polarimeter_coefficients(dataupper[*,icurrent],varsupper[*,icurrent],pols, $
				xyb_upper)
		lowercoefs[i,*]=get_polarimeter_coefficients(datalower[*,icurrent],varslower[*,icurrent],pols, $
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
	if(n_elements(diag_plot_dir) gt 0) then plot_cal_comblk_data, diag_plot_dir

	print,chi2*penalty,input ; Printout to show progress of inversion.
	return,chi2*penalty ; Done.
	
end


;+
; Initialize the common block used by comp_cal_powfunc, which is called by Amoeba or
; Powell to compute the crosstalk coefficients and corresponding chi squared for a given
; input stokes vector and calibration optic configuration. This involves sifting though
; the cal files, organizing them according to the instrument polarization analyzer state
; (i.e., nominally I+Q, I-Q, I+U, etc), and assigning them into the correct spot in the 
; common block (Amoeba and Powell force the use of a common block for data and constants).
;
; Inputs to this procedure are as follows:
;	cal_directory: The directory containing the cal files
;	wave: Which line to use (median wavelength rounded to nearest nm)
;	beam: Which beam splitter setting to use (+1 or -1)
;
; The common block has the following components:
;	xybasis:	The set of functions which define the spatial variation of the crosstalk 
;				(see comp_cal_xybasis), dimensions are [nx,ny,nbasis], where nbasis is 4 by default.
;	xmat:		Array of x pixel coordinates corresponding to xybasis.
;	ymat:		Array of y pixel coordinates corresponding to xybasis.
;	data:		The data. There is one image for each unique combination of polarization analyzer 
;				configuration (I+Q, I+U, etc) and cal optics configuration (polarizer present,
;				retarder present, polarizer angle). Multiple images which have the same analyzer
;				and cal optics configuration are averaged. Dimensions are [nx,ny,n_upols*n_ucals],
;				where n_upols and n_ucals are the number of unique analyzer and calibration optics
;				configurations, respectively.
;	vars:		The variances corresponding to the data, estimated by assuming shot noise dominates.
;				There is one image for each unique combination of polarization analyzer configuration
;				(I+Q, I+U, etc) and cal optics configuration (polarizer present,
;				retarder present, polarizer angle).
;	mask:		Mask image [nx,ny] indicating which pixels are good. This is determined from the flats
;				and by comparison to a median smoothing.
;	uppermask:	Mask of pixels above the diagonal (ymat gt xmat).
;	lowermask:	Mask of pixels below the diagonal (xmat gt ymat).
;	xyb_upper:	Trimmed down version of xybasis containing only those values for the unmasked
;				pixels above the diagonal. This and all other 'upper' arrays replace the spatial
;				indices (two of them, one for x and one for y) with a single index based on
;				where(mask*uppermask) - roughly, xyb_upper[*,i] = xybasis_i[where(mask*uppermask)],
;				with xybasis_i = xybasis[*,*,i]. We work only with these arrays for the most 
;				time intensive part of the calculation, speeding up execution.
;	xyb_lower:	Trimmed down version of xybasis containing only those values for the unmasked
;				pixels below the diagonal. This and all other 'lower' arrays replace the spatial
;				indices (two of them, one for x and one for y) with a single index based on
;				where(mask*lowermask).
;	dataupper:	Trimmed down version of data containing only those values for the unmasked
;				pixels above the diagonal.
;	dataupper:	Trimmed down version of data containing only those values for the unmasked
;				pixels below the diagonal.
;	varsupper:	Trimmed down version of variances containing only those values for the unmasked
;				pixels above the diagonal.
;	varslower:	Trimmed down version of variances containing only those values for the unmasked
;				pixels below the diagonal.
;	cpols:		Array giving the cal polarizer state (1 = in, 0 = out) for each image in the data.
;	pangs:		Array giving the cal polarizer angle for each image in the data.
;	crets:		Array giving the cal retarder state (1 = in, 0 = out) for each image in the data.
;	upols:		String array listing each unique polarization analyzer configuration
;				(i.e., I+Q, I+U, etc) present.
;	ucals:		String array listing each unique calibration optics configuration
;				(i.e., I+Q, I+U, etc) present.
;	datapols:	String array listing the polarization analyzer configuration for each image in data.
;	datacals:	String array specifying the calibration optics configuration for each image in data.
;				the labels consist of a polarizer state flag (1 = in, 0 = out), retarder state flag
;				(1 = in, 0 = out), and the polarizer angle listed in the file, rounded to the nearest
;				degree.
;	cal_data:	When comp_cal_powfunc is run, it will store its fit to the data in this array.
;				Has the same dimensions as data.
;	uppercoefs:	When comp_cal_powfunc is run, it will store the coefficients for the fit above the diagonal
;				in this array.
;	lowercoefs:	When comp_cal_powfunc is run, it will store the coefficients for the fit below the diagonal
;				in this array.
;	nstokes:	The number of components in a stokes vector (will almost always be 4 - this is only placed
;				here to clean up the code a bit).
;
; Author: Joseph Plowman
;-
pro init_powfunc_comblk, cal_directory, wave, beam, config_filename=config_filename

	common comp_cal_comblk, xybasis, xyb_upper, xyb_lower, dataupper, datalower, varsupper, varslower, $
			xmat, ymat, cpols, pangs, crets, upols, datapols, datacals, cal_data, uppercoefs, $
			lowercoefs, uppermask, lowermask, data, vars, mask, nstokes, ucals, calvars, calvar_solve
	
	s=replicate(1,5,5) ; Array for 5x5 erode function to pad the mask.
	nstokes=4 ; Stokes vector has 4 components,
	photfac = double(1.0/sqrt(875.0)) ; Conversion factor from disk intensity to photons (for 250ms exposures)

	; Initialize the paths and common blocks for CoMP pipeline routines
	date_dir = file_basename(cal_directory)	
	comp_configuration, config_filename=config_filename
	comp_initialize,  date_dir

	; Standard CoMP images are 1k by 1k, before beam combining:
	if(n_elements(nx) eq 0) then nx = 1024
	if(n_elements(ny) eq 0) then ny = 1024

	; Find the calibration files in the cal directory:
	files = file_search(cal_directory+'*.FTS')
	cal_info = get_cal_info(files)
	
	; The files which have the correct line ('wave'), are not darks ('cvers') and are labeled as cal data:
	calfiles = where(cal_info.waves eq wave and cal_info.cvers and cal_info.cpols)
	nfiles = n_elements(calfiles) ; Total number of cal files.
	
	n_mpols = total(cal_info.mpols ne '',2) ; The number polarization analyzer states in each file.
	
	; Find the unique measured polarizations:
	upols = cal_info.mpols[uniq(cal_info.mpols,sort(cal_info.mpols))]
	upols = upols[where(upols ne '')]
	n_upols = n_elements(upols)
	
	; Find the unique cal optics configurations:
	ucals = cal_info.ctags[uniq(cal_info.ctags,sort(cal_info.ctags))]
	n_ucals = n_elements(ucals)

	; Total number of images needed for all unique combinations of polarization analyzer and calibration
	; optics configurations:
	ndata = total(n_upols*n_ucals)

	; Arrays which will label the polarization analyzer and calibration optics states for each
	; image in the data array:
	datapols = strarr(ndata)
	datacals = strarr(ndata)
	for i=0,n_upols-1 do begin
		for j=0,n_ucals-1 do begin
			datapols[i*n_ucals-j]=upols[i]
			datacals[i*n_ucals-j]=ucals[j]
		endfor
	endfor
	
	; Compute the spatial basis images:
	xybasis=comp_cal_xybasis(nx=nx,ny=ny,xmat=xmat,ymat=ymat)
	uppermask=ymat gt xmat ; mask for pixels above the diagonal...
	lowermask=xmat gt ymat ; mask for pixels below the diagonal...
	nbasis = n_elements(xybasis[0,0,*])
	
	data = fltarr(nx,ny,ndata) ; Array containing data for each unique configuration.
	vars = fltarr(nx,ny,ndata) ; Variances corresponding to data (assumes only shot noise).

	navgs = fltarr(ndata) ; How many individual CoMP exposures went into each image in data.
	cpols = dblarr(ndata) ; Polarization analyzer flags for each image in data.
	crets = dblarr(ndata) ; Calibration retarder flags for each image in data.
	pangs = dblarr(ndata) ; Calibration polarizer angle for each image in data.
	datacount = lonarr(ndata) ; Counts how many images were averaged for each element of data.		
	mask=uppermask or lowermask ; Initialize the mask.
	
	
	for i=0,nfiles-1 do begin
		ii=calfiles[i] ; Select the next cal file.
		comp_read_data, cal_info.files[ii], images, headers, header0 ; Read the file...
		comp_apply_flats_darks,images,headers,date_dir, flat_header=flat_header; Apply flats & darks.
		cal = cal_info.ctags[ii] ; Cal optics state for this file (assumes only one per file).
		
		; Loop over the measured polarizations in the file:
		for j=0,n_mpols[ii]-1 do begin
			pol = cal_info.mpols[ii,j] ; The current measured polarization.
			; Where this polarization and calibration combo lands in the data array (which index):
			idata = where(datapols eq pol and datacals eq cal)

			; Get the current file's data for this polarization analyzer state:
			datai = comp_get_component(images, headers, pol, beam, $
                                                   /average_wavelengths, $
                                                   headersout=headersout)
                        ; Add it do the data and variance arrays (weighting by
                        ; the number of exposures):
                        ; TODO: not sure this is correct
                        dims = size(headersout, /dimensions)
                        for d = 0L, dims[1] - 1L do begin
                          h = headersout[*, d]
                          data[*, *, idata] += datai * sxpar(h, 'NAVERAGE')
                          vars[*, *, idata] += photfac * abs(datai) * sxpar(h, 'NAVERAGE')
                          ; increment the total number of exposures (so we can
                          ; renormalize later)
                          navgs[idata] += sxpar(h, 'NAVERAGE')
                        endfor
			datacount[idata] += 1
			
			; Make sure we've recorded book keeping data for this index:
			cpols[idata]=cal_info.cpols[ii]
			crets[idata]=cal_info.crets[ii]
			pangs[idata]=cal_info.cangs[ii]
			
			comp_make_mask_1024, date_dir, flat_header, mask0 ; Compute the mask for this file.
			mask *= erode(mask0,s) ; Pad the file's mask and combine it with the main mask.
                        ; TODO: also not sure about this
                        dims = size(datai, /dimensions)
                        for d = 0L, dims[2] - 1L do begin
                          ; remove outlier pixels via median filtering
                          mask *= abs(datai[*, *, d] / median(datai[*, *, d], 3) - 1.0) lt 0.25
                        endfor
		endfor
	endfor
	
	; If some calibration and polarizer analyzer combinations are absent from the data, remove them
	; from the common block arrays:
	wherepresent=where(datacount gt 0)
	data=data[*,*,wherepresent]
	vars=vars[*,*,wherepresent]
	navgs=navgs[wherepresent]
	cpols=cpols[wherepresent]
	crets=crets[wherepresent]
	pangs=pangs[wherepresent]
	ndata=total(datacount gt 0)
	datacount = datacount[wherepresent]
	
	; Initialize flattened upper and lower arrays, used to speed some parts of calculation:
	whereupper = where(uppermask*mask,npix_upper)
	wherelower = where(lowermask*mask,npix_lower)
	dataupper=dblarr(npix_upper,ndata)
	datalower=dblarr(npix_lower,ndata)
	varsupper=dblarr(npix_upper,ndata)
	varslower=dblarr(npix_lower,ndata)
	xyb_upper=dblarr(npix_upper,nbasis)
	xyb_lower=dblarr(npix_lower,nbasis)
	
	; Assign values to flattened upper and lower basis function arrays:
	for i=0,nbasis-1 do begin
		xyb_i = reform(xybasis[*,*,i])
		xyb_upper[*,i]=xyb_i[whereupper]
		xyb_lower[*,i]=xyb_i[wherelower]
	endfor
	
	for i=0,ndata-1 do begin
		; Renormalize the data by total exposure time:
		datai = reform(data[*,*,i]/navgs[i]);/datacount[i])
		varsi = reform(data[*,*,i]/navgs[i]);/datacount[i])	
		data[*,*,i] = datai
		vars[*,*,i] = varsi
		
		; Assign values to flattened upper and lower data and variance arrays:
		dataupper[*,i]=datai[whereupper]
		datalower[*,i]=datai[wherelower]
		varsupper[*,i]=varsi[whereupper]
		varslower[*,i]=varsi[wherelower]
	endfor
	
	; All done.
	
end
