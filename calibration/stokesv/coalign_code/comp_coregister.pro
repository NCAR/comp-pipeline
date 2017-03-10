; Convert from image pixel coordinates (x_pix_in, y_pix_in) to arcseconds (x_arcsecs, y_arcsecs), 
; given the following input parameters:
; 
;	center: The image center in arseconds
;	scale: The pixel scale of the image
;	rollcen: The roll center of the image (in arcseconds)
;	rota: The rotation angle of the image about rollcen, in degrees.
;	nx: The number of pixels along the x direction (i.e., scale[0])
;	ny: The number of pixels along the y direction (i.e., scale[1])
pro pix_to_arcsecs, center, scale, rollcen, rota, nx, ny, x_pix_in, y_pix_in, x_arcsecs, y_arcsecs

	tilt = rota*2.0*acos(0.0)/180.0

	; Translate from corner coords:
	x_pix = x_pix_in - 0.5*(nx-1.0)
	y_pix = y_pix_in - 0.5*(ny-1.0)
	
	; Convert to arcseconds and translate from image center to roll center:
	x_arcsecs0 = x_pix*scale[0] + center[0] - rollcen[0]
	y_arcsecs0 = y_pix*scale[1] + center[1] - rollcen[1]
	
	; Apply roll rotation:
	x_arcsecs = x_arcsecs0*cos(tilt) - y_arcsecs0*sin(tilt)
	y_arcsecs = x_arcsecs0*sin(tilt) + y_arcsecs0*cos(tilt)

	; Translate back from roll center:
	x_arcsecs = x_arcsecs+rollcen[0]
	y_arcsecs = y_arcsecs+rollcen[1]

end

; Convert from arcseconds (x_arcsecs_in, y_arcsecs_in) to image pixel coordinates (x_pix, y_pix), 
; given the following input parameters:
; 
;	center: The image center in arseconds
;	scale: The pixel scale of the image
;	rollcen: The roll center of the image (in arcseconds)
;	rota: The rotation angle of the image about rollcen, in degrees.
;	nx: The number of pixels along the x direction (i.e., scale[0])
;	ny: The number of pixels along the y direction (i.e., scale[1])
pro arcsecs_to_pix, center, scale, rollcen, rota, nx, ny, x_arcsecs_in, y_arcsecs_in, x_pix, y_pix

	tilt = -rota*2.0*acos(0.0)/180.0

	; Translate to roll center:
	x_arcsecs = x_arcsecs_in-rollcen[0]
	y_arcsecs = y_arcsecs_in-rollcen[1]
	
	; Apply roll rotation:
	x_arcsecs0 = x_arcsecs*cos(tilt) - y_arcsecs*sin(tilt)
	y_arcsecs0 = x_arcsecs*sin(tilt) + y_arcsecs*cos(tilt)

	; Translate from roll center to image center and convert to pixels:
	x_pix = (x_arcsecs0-center[0]+rollcen[0])/scale[0]
	y_pix = (y_arcsecs0-center[1]+rollcen[1])/scale[1]
	
	; Translate to corner coords:
	x_pix += 0.5*(nx-1.0)
	y_pix += 0.5*(ny-1.0)
		
end

; Returns an array containing the conversion from AIA photons to DN for an input list of
; by channels, which are assumed to be an array of string labels like those returned by
; aia_get_response. Taken from Solar Physics 275, 41. 
function aia_firdem_dnperphot, channels

	refchannels=['A94', 'A131', 'A171', 'A193', 'A211', 'A304', 'A335']
	refg = [2.128,1.523,1.168,1.024,0.946,0.658,0.596]
	nchan=n_elements(channels)
	g=dblarr(nchan)
	for i=0,nchan-1 do begin
		g(i) = refg(where(refchannels eq channels(i)))
	endfor
	
	return,g
	
end

;+
; Name: aia_firdem_readnoise
;
; Returns an array containing standard deviation of the AIA read noise for an input list of
; channels, which are assumed to be an array of string labels like those returned by
; aia_get_response. Taken from Solar Physics 275, 41. 
;
;	--Joseph Plowman (plowman@physics.montana.edu) 09/17/12
;-
function aia_firdem_readnoise, channels

	refchannels=['A94', 'A131', 'A171', 'A193', 'A211', 'A304', 'A335']
	refn = [1.14,1.18,1.15,1.2,1.2,1.14,1.18]
	nchan=n_elements(channels)
	n=dblarr(nchan)
	for i=0,nchan-1 do begin
		n(i) = refn(where(refchannels eq channels(i)))
	endfor
	
	return,n
	
end

;+
; Name: aia_firdem_estimate_error
;
; Estimates the AIA error in a data array (units of DN), assuming shot and read noise specified
; by Solar Physics 275, 41. 
;
; Inputs:
;	
;	channels:	The channels labels for the data, which are assumed to be like those returned by
;				aia_get_response.
;	data:		The data array (dimensions [nx,ny,n_elements(channels)]) whose error is to be
;				estimated.
;
;	--Joseph Plowman (plowman@physics.montana.edu) 09/17/12
;-
function aia_firdem_estimate_error, channels, data

	nchan = n_elements(channels)
	dnpp = aia_firdem_dnperphot(channels) ; Photon to DN conversion.
	rdn = aia_firdem_readnoise(channels) ; Read noise.
	sigmas=data
	for i=0,nchan-1 do begin
		sigmas(*,*,i) = sqrt((data(*,*,i)*dnpp(i)) > 0.0*rdn(i) + 0.0 + rdn(i)^2)
	endfor
	
	return,sigmas
	
end

;+
; Resample an image whose orientation and scale are specified by the structure index. The orientation and scale
; of the new image are defined in terms of the following parameters:
;	
;	center: The image center for the new image ([x,y])
;	scale: The pixel scale for the new image ([dx,dy])
;	nx: The number of pixels in the x direction
;	ny: The number of pixels in the y direction
;	rollcen: The roll center for the image rotation ([x,y]) (optional keyword)
;	rota: The roll angle for the image rotation, in degrees (optional keyword)
;
; The index structure must also specify these parameters, via elements xc,yc (for center), dx, dy (for scale)
; roll_center (for rollcen), and roll_angle (for rota). Any resample parameters not specified on input
; are set to be the same as in the index structure. The resampling is done using IDL's 'interpolate'
; function with cubic=-0.5
; 
; An output index containing the new parameters defined above, along the exposure time (but nothing else)
; will be returned in the optional keyword indexout.
;-
function resample_image, image, index, center, scale, nx, ny, rollcen=rollcen, rota=rota, smooth_radius = smooth_radius, indexout=indexout 

	; Orientation, scale, and dimensions of input image:
	nx0 = n_elements(image[*,0])
	ny0 = n_elements(image[0,*])
	if(where(tag_names(index) eq 'XC') eq -1) then begin
		center0 = [index.crval1,index.crval2]
		scale0 = [index.cdelt1,index.cdelt2]
		pix_to_arcsecs, center0, scale0, [0.0,0.0], 0.0, nx0, ny0, index.crpix1, index.crpix2, rollcenx, rollceny
		rollcen0 = [rollcenx,rollceny]
		rota0 = index.crota2
	endif else begin		
		center0 = [index.xc,index.yc]
		scale0 = [index.dx, index.dy]
		rollcen0 = index.roll_center
		rota0 = index.roll_angle
	endelse
	
	; Any resample parameters not set on input are assigned to be the same as the input image:
	if(n_elements(center) eq 0) then center = center0
	if(n_elements(scale) eq 0) then scale = scale0
	if(n_elements(nx) eq 0) then nx = nx0
	if(n_elements(ny) eq 0) then ny = ny0
	if(n_elements(rollcen) eq 0) then rollcen = rollcen0
	if(n_elements(rota) eq 0) then rota = rota0

	; Arrays containing pixel coordinates of the new image:
	xpix = dblarr(nx,ny)
	ypix = dblarr(nx,ny)

	for i=0,ny-1 do xpix[*,i] = findgen(nx)
	for i=0,nx-1 do ypix[i,*] = findgen(ny)

	; Compute arcsecond coordinates of each pixel in new image:
	pix_to_arcsecs, center, scale, rollcen, rota, nx, ny, xpix, ypix, xa, ya
	
	; Convert these to the pixel coordinates WRT the original image
	arcsecs_to_pix, center0, scale0, rollcen0, rota0, nx0, ny0, xa, ya, x_pix0, y_pix0
	
	; Create the output index structure if it has been specified:
	if(n_elements(indexout) gt 0) then begin
		exptimes = interpolate(index.exptimes,x_pix0,y_pix0,cubic=-0.5)
		indexout = {nx:nx, ny:ny, xc:center[0], yc:center[1], dx:scale[0], dy:scale[1], roll_center:rollcen, roll_angle:rota, exptimes:exptimes, exptime0:index.exptime0}
	endif
	
	; Create and return the resampled image:
	return, interpolate(image,x_pix0,y_pix0,cubic=-0.5)

end

;+ 
; This is the goodness of fit function called via IDL's Powell method optimizer. It rotates and shifts the
; image to be coaligned according to the parameters in xin [xcenter, ycenter, rotation], then computes a
; modified chi squared between the rotated/shifted image and the reference image. Other information needed
; for the transformation, contained in the common block comp_coreg_comblk, are the following:
;
;	img0: The reference image.
;	img1: The image which is being coaligned with the reference image (not modified by this code).
;	err0: Uncertainty weighting for the reference image.
;	err1: Uncertainty weighting for the coalignment image.
;	index0: Structure defining the orientation and scale for the reference image.
;	index1: Structure defining the orientation and scale for the coalignment image.
;	mask: Pixel mask for which pixels (in reference/coaligned) image to use for coalignment.
;	rsmooth: Smoothing radius for the images (e.g., for noisy images).
;	chi2: Place to store the most recent computed chi squared.
;
; Chi squared is modified by omitting pixels where mask=0 and by rescaling the masked images so that they
; have the same median. The most extreme (95th percentile and above) residuals are also omitted from the chi
; squared. A minimum error of 0.1 in individual pixels is enforced, so that pixels with unusually and 
; erroneously small errors do not drive the coalignment.
function comp_coreg_pwlfunc, xin

	; Common block for passing additional arguments around IDL's powell procedure:
	common comp_coreg_comblk, n_unmasked, unmasked, img0, img1, err0, err1, index0, index1, mask, rsmooth, chi2, niter
	
	; Parameters for the image resampling:
	scale = [index1.cdelt1,index1.cdelt2]
	center = [xin[0],xin[1]]
	tilt = xin[2]
		
	nx = n_elements(img0[*,0])
	ny = n_elements(img0[0,*])
	xc0 = index1.crval1
	yc0 = index1.crval2
	
	; Index structure for the coalignment image, prior to resampling:
	index = {dx:index1.cdelt1, dy:index1.cdelt2, xc:xc0, yc:yc0, roll_center:[xc0,yc0], roll_angle:index1.crota2}
	
	; Rotate and shift the coalignment image and associated errors:
	img1_resamp = resample_image(img1, index, center, scale, nx, ny, rollcen = center, rota=tilt, smooth_radius=rsmooth)	
	err1_resamp = resample_image(err1, index, center, scale, nx, ny, rollcen = center, rota=tilt, smooth_radius=rsmooth)
	
	; Rescale the residuals and errors so that the images have the same median:
	med1 = mean(img1_resamp[unmasked])
	med0 = mean(img0[unmasked])	
	errs2 = (err1_resamp/med1)^2+(err0/med0)^2	
	resids = (img1_resamp/med1-img0/med0)^2.0/errs2	
	
	; Calculate chi squared, omitting the most extreme residuals (90th percentile and above):
	resids[unmasked] *= (abs(resids[unmasked]) lt estimate_quantile(abs(resids[unmasked]),0.90))	
	chi2 = total(resids[unmasked])/(n_unmasked*0.9)
	
	niter++
	
	return, chi2; > 0.5
	
end

; Resample the final coaligned image. Does the same thing as comp_coreg_pwlfunc, but returns
; the coaligned image (and optionally an output index) rather than a chi squared. Uses the same common
; block as comp_coreg_pwlfunc
function comp_coreg_final, xin, indexout=indexout

	common comp_coreg_comblk, n_unmasked, unmasked, img0, img1, err0, err1, index0, index1, mask, rsmooth, chi2, niter
	
	scale = [index1.cdelt1,index1.cdelt2]
	center = [xin[0],xin[1]]
	tilt = xin[2]
		
	nx = n_elements(img0[*,0])
	ny = n_elements(img0[0,*])
	xc0 = index1.crval1
	yc0 = index1.crval2
	
	index = {dx:index1.cdelt1, dy:index1.cdelt2, xc:xc0, yc:yc0, roll_center:[xc0,yc0], roll_angle:index1.crota2}
	
	img1_resamp = resample_image(img1, index, center, scale, nx, ny, rollcen = center, rota=tilt, smooth_radius=rsmooth)	

	indexout = index1
	indexout.crval1=xin[0]
	indexout.crval2=xin[1]
	indexout.crota2=tilt
	
	return,img1_resamp
	
end


; Resample the coaligned image. Does the same thing as comp_coreg_pwlfunc, but returns
; the coaligned image (and optionally an output index) rather than a chi squared. Uses the an input index
; rather than taking it from comp_coreg_comblk:
function comp_coreg_resamp, img1, index1, index0, xin, indexout=indexout

	scale = [index1.cdelt1,index1.cdelt2]
	center = [xin[0],xin[1]]
	tilt = xin[2]
		
	nx = n_elements(img1[*,0])
	ny = n_elements(img1[0,*])
	xc0 = index1.crval1
	yc0 = index1.crval2
	
	index = {dx:index1.cdelt1, dy:index1.cdelt2, xc:xc0, yc:yc0, roll_center:[xc0,yc0], roll_angle:index1.crota2}
	
	img1_resamp = resample_image(img1, index, center, scale, nx, ny, rollcen = center, rota=tilt, smooth_radius=rsmooth)	

	indexout = index1
	indexout.crval1 = index0.crval1
	indexout.crval2 = index0.crval2
	indexout.crota2 = index0.crota2
	
	return,img1_resamp

end

;-
; Coregister (coalign) one CoMP image (img1_in) with a reference image (img0_in). Coalignment is optimized
; over shifts and rotations - pixel sizes are not changed. Has the following inputs:
;
;	img0_in: A reference image to coalign against.
;	img1_in: The image to be coaligned.
;	index0_in: Structure which specifies the orientation and scale of the reference image.
;	index1_in: Structure which specifies the orientation and scale of the coalignment image.
;	mask_in: Mask image which indicates which pixels are valid and invalid (non-zero is valid).
;	err0_in: Image of error estimates for the reference image.
;	err1_in: Image of error estimates for the coalignment image.
;	smooth_radius: Smooth the images with this radius when coaligning (May help avoid local minima)
;	guess: Optional keyword giving an initial guess for the coalignment parameters.
;
; Returns a vector of coalignment parameters consisting of a corrected x center, y center and rotation angle
; (in degrees) for the image ([xcen,ycen,rota]). Optionally also returns, via keyword:
;
;	img1_out: The coaligned image
;	index1_out: (minimal) index structure for the coaligned image. Contains only the new image's orientation
;	and scale information.
;-
function comp_coregister, img0_in, img1_in, index0_in, index1_in, mask_in, err0_in=err0_in, err1_in=err1_in, smooth_radius=smooth_radius, guess=guess, img1_out=img1_out, err1_out=err1_out, index1_out=index1_out, show_result=show_result

	; Common block for the function which computes goodness of fit (comp_coreg_pwlfunc) for the coalignment.
	; This function is called via IDL's 'powell' procedure. Description of elements of the common block is
	; found in the comments for comp_coreg_pwlfunc.
	common comp_coreg_comblk, n_unmasked, unmasked, img0, img1, err0, err1, index0, index1, mask, rsmooth, chi2, niter
	
	niter=0
	
	index0=index0_in
	index1=index1_in
	mask = mask_in

		
	scale = [index1.cdelt1, index1.cdelt2]
	center = [index0.crval1,index0.crval2]
	nx = n_elements(img1_in[*,0])
	ny = n_elements(img1_in[*,0])
	img0 = resample_image(img0_in, index0_in, center, scale, nx, ny, smooth_radius=rsmooth)
	index0.cdelt1=scale[0]
	index0.cdelt2=scale[1]
	index0.naxis1=nx
	index0.naxis2=ny

	img0 = float(img0 > 0)
	img1 = float(img1_in > 0)
	if(n_elements(smooth_radius) gt 0) then rsmooth = smooth_radius

	; If no input errors are supplied, assign some plausible error values based on the range of the image
	; and the assumption of shot noise:
	if(n_elements(err0_in) eq 0) then begin
		img0hi = estimate_quantile(img0[where(mask ne 0 and img0 gt 0)],0.995)
		img0lo = estimate_quantile(img0[where(mask ne 0 and img0 gt 0)],0.005)
		err0_in = 0.0025*img0*sqrt(img0hi/(img0+0.0001*img0lo)) + 0.5*img0lo
	endif
		
	if(n_elements(err1_in) eq 0) then begin
		img1hi = estimate_quantile(img1[where(mask ne 0 and img1 gt 0)],0.995)
		img1lo = estimate_quantile(img1[where(mask ne 0 and img1 gt 0)],0.005)
		err1_in = 0.0025*img1*sqrt(img1hi/(img1+0.0001*img1lo)) + 0.5*img1lo
	endif
		
	err0 = err0_in
	err1 = err1_in
			
	; Smooth the image and errors if a smooth radius is specified:
	if(n_elements(smooth_radius) gt 0) then begin
		smooth_width_px = smooth_radius
		kernel_xa = -5.0*smooth_width_px + findgen(10*smooth_width_px+1)
		kernel = exp(-0.5*kernel_xa^2/smooth_width_px^2)/(4.0*acos(0)*smooth_width_px^2)
		img0 = convol(float(img0),kernel,/edge_truncate,/normalize)
		img1 = convol(float(img1),kernel,/edge_truncate,/normalize)
		err0 = convol(float(err0),kernel,/edge_truncate,/normalize)
		err1 = convol(float(err1),kernel,/edge_truncate,/normalize)
	endif

	; Find the pixels that aren't masked out:
	unmasked = where(mask ne 0 and img0 gt 0, n_unmasked)
	
	; Set the minimum error level to 5%:
	smallerrs = where(err0 lt 0.001*img0, count)
	if(count gt 0) then err0[smallerrs] = 0.001*img0[smallerrs]
	smallerrs = where(err1 lt 0.001*img1, count)
	if(count gt 0) then err1[smallerrs] = 0.001*img1[smallerrs]

	; Initial guess for shift and rotation parameters - assume headers are correct:
	if(n_elements(guess) eq 0) then guess = [index0.crval1,index0.crval2,index0.crota2]

	; Set up and run the Powell's method search for optimal coalignment parameters:
	x0 = guess	
	xi = identity(3,/double)
	
	print,'Initial chi squared, x shift, y shift, and rotation:'
	print,comp_coreg_pwlfunc(x0),0,x0
	
	powell, x0, xi, 1.0e-3, fmin, "comp_coreg_pwlfunc", /double
		
	; If an output image is requested, compute it using the coaligment parameters:
	if(n_elements(img1_out) gt 0 or keyword_set(show_result)) then begin
		img1_out=comp_coreg_resamp(img1, index1, index0, x0, indexout=index1_out)
		err1_out=comp_coreg_resamp(err1, index1, index0, x0)
	endif
	
	if(keyword_set(show_result)) then begin
		!p.multi=[0,2,1]
		img_color = dblarr(nx,ny,3)
;		img1_out=comp_coreg_final(x0,indexout=index1_out)
		img_color[*,*,0] = img1_out*float(mask_in)/mean(img1_out[where(mask_in gt 0)],/nan)
		img_color[*,*,2] = img0*float(mask_in)/mean(img0[where(mask_in gt 0)],/nan)
		img_color[*,*,1] = 0.5*(img_color[*,*,0]+img_color[*,*,2])
		img_color2 = dblarr(nx,ny,3)
		img_color2[*,*,0] = img1*float(mask_in)/mean(img1_out[where(mask_in gt 0)],/nan)
		img_color2[*,*,2] = img0*float(mask_in)/mean(img0[where(mask_in gt 0)],/nan)
		img_color2[*,*,1] = 0.5*(img_color2[*,*,0]+img_color2[*,*,2])
		plot_image,img_color2,min=0,max=estimate_quantile(img_color[unmasked],0.995),title='AIA (Blue) + CoMP (red) - Unaligned'
		plot_image,img_color,min=0,max=estimate_quantile(img_color[unmasked],0.995),title='AIA (Blue) + CoMP (red) - Aligned'
	endif
	
	guess = x0	

	print,'Final chi squared, iteration count, x shift, y shift, and rotation:'
	print, chi2, niter, x0
	
	return, [x0,chi2]
	
end

;+
; Read a CoMP image from the files, compute errors, and otherwise do the mundane things required
; to prepare for coalignment. Arguments:
;	
;	image: Contains the image for alignment on output.
;	err: Contains the error image for alignment on output.
;	index: Contains the index structure for the alignment image on output.
;	file: The fits file to load the images from. Must be input if imagearr, headerarr, and header0 are not.
;	imagearr: The array of images for the image, as returned by comp_read_data. Used as input only if
;		file is not input.
;	headerarr: The array of headers for the image, as returned by comp_read_data. Used as input only if
;		file is not input.
;	header0: The header for the 0th extension. Used as input only if
;		file is not input.
;	mask: The good pixel mask, which will be computed from the header.
;	midtune: Returns the number of the tuning used for alignment (the middle one).
;-
pro comp_coalign_image_setup, image, err, index, file=file, imagearr=imagearr, headerarr=headerarr, $
		header0=header0, mask=mask, midtune=midtune
		
	if(n_elements(file) ne 0) then comp_read_data, file, imagearr, headerarr, header0

	index0 = convert_ascii_header(header0)
	if(tag_exist(index0, 'NTUNE')) then ntune = index0.ntune
	if(tag_exist(index0, 'NTUNES')) then ntune = index0.ntunes
	
	errarr = sqrt(comp_estimate_var(imagearr,headerarr,ntune))
	
	midtune = floor(0.5*ntune)
	index = convert_ascii_header(headerarr[*,midtune])
	image = imagearr[*,*,midtune]
	err = errarr[*,*,midtune]

	; Why does comp_make_mask require a date in addition to the header??
	date = strjoin(strsplit(sxpar(header0,'DATE-OBS'),'-',/extract))
	comp_make_mask2,date,header0,mask,occ_fac=1.05,fld_fac=0.935

end


;+
; Load and coalign the CoMP image contained in file1 with that in file0.
; Inputs:
;	file0: Reference image filename.
;	file1: Filename of image to coalign with reference image.
;	guess: Guess to use for coalignment.
;	mask: Good pixel mask.
;	show_result: Show the results of the alignment.
;	reset: Don't use the previously store image for alignment, and use only 2 images instead.
;
; Outputs:
;	img1_out: Coaligned output image.
;	err1_out: Coaligned error image.
;	index1_out: Minimal image index with updated coalignment parameters.
;
; Returns a vector of coalignment parameters: [xcenter, ycenter, rotation] (in arc seconds and degrees,
; respectively).
;-
function comp_coalign_2files, file0, file1, guess=guess, img1_out=img1_out, err1_out=err1_out, $
		index1_out=index1_out, show_result=show_result, mask=mask, reset=reset

	comp_coalign_image_setup,image0,err0,index0,file=file0,mask=mask
	comp_coalign_image_setup,image1,err1,index1,file=file1

	coalign_parms = comp_coregister(image0, image1, index0, index1, mask, err0_in=err0, $
		err1_in=err1, guess=guess, img1_out=img1_out, err1_out=err1_out, $
		index1_out=index1_out, show_result=show_result)
	
	return, coalign_parms

end

pro write_aligned_jpg_sequence, filenames, iref, parameters, mask, headstr, tuning=tuning
	
	nfile = n_elements(filenames)
	
	comp_read_data,filenames[iref],imgarr_ref, headerarr_ref, header0_ref
	index0 = convert_ascii_header(header0_ref)
	if(tag_exist(index0, 'NTUNE')) then n_tunings = index0.ntune
	if(tag_exist(index0, 'NTUNES')) then n_tunings = index0.ntunes
	refindex = convert_ascii_header(headerarr_ref[*,floor(0.5*n_tunings)])

	for i=0,nfile-1 do begin
		comp_read_data,filenames[i],imagearr,headerarr,header0
		index0 = convert_ascii_header(header0)
		if(tag_exist(index0, 'NTUNE')) then n_tunings = index0.ntune
		if(tag_exist(index0, 'NTUNES')) then n_tunings = index0.ntunes
		tuning = floor(0.5*n_tunings)
		image = imagearr[*,*,tuning]
		index = convert_ascii_header(headerarr[*,tuning])
		if(n_elements(imagemax) eq 0) then begin
			imagemax = estimate_quantile(image[where(mask)]/index.exposure,0.99)
		end
		imageout = comp_coreg_resamp(image, index, refindex, parameters[i,*], indexout=indexout)
		imageout = imageout/index.exposure < imagemax
		outfilename = headstr+'_'+string(format='(%"%03d.jpg")',i)
		write_jpeg,outfilename,((mask*imageout < 1) > 0)*255/imagemax,quality=100
		print,i
	endfor
	
end

	

function comp_coregister_aiawrapper, image, index, aiadir, mask, guess=guess, show_result=show_result, comperr=comperr

	date = index.dateobs
	time = index.timeobs

	ccsdstime = date+'T'+time+'.500Z'
	
	aiasearch = vso_search(near=ccsdstime,inst='aia',wave='211 Angstrom')
	a=vso_get(aiasearch, out_dir=aiadir, filenames=aiafile)
	print,ccsdstime,aiafile
	aia_prep,aiafile,[0],aiaindex,aiadata,/uncomp_delete
	
	aiaerr = aia_firdem_estimate_error('A211', aiadata)
	
	return, comp_coregister(aiadata, image, aiaindex, index, mask, guess=guess, show_result=show_result, err0_in=aiaerr, err1_in=comperr)

end
