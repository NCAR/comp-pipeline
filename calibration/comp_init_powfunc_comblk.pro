; docformat = 'rst'

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
pro comp_init_powfunc_comblk, cal_directory, wave, beam, config_filename=config_filename

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
                        data[*, *, idata] += datai * sxpar(headersout, 'NAVERAGE')
                        vars[*, *, idata] += photfac * abs(datai) * sxpar(headersout, 'NAVERAGE')
                        ; increment the total number of exposures (so we can
                        ; renormalize later)
                        navgs[idata] += sxpar(headersout, 'NAVERAGE')
			datacount[idata] += 1

			; make sure we've recorded book keeping data for this index
			cpols[idata]=cal_info.cpols[ii]
			crets[idata]=cal_info.crets[ii]
			pangs[idata]=cal_info.cangs[ii]

                        ; compute the mask for this file
			comp_make_mask_1024, date_dir, flat_header, mask0
                        ; pad the file's mask and combine it with the main mask
			mask *= erode(mask0,s)
                        ; remove outlier pixels via median filtering
                        mask *= abs(datai / median(datai, 3) - 1.0) lt 0.25
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
