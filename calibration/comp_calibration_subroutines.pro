;+
; Function to scan a set of files and return a structure listing the calibration files contained
; and their details. Input:
;	
;	files: string array of the filenames to check.
;
; Optional keyword arguments:
;	npchans: Number of polarization channels expected (defaults to 6)
;	whichwave: Which line to check for - median wavelength rounded to nearest nanometer.
;
; Returns: structure detailing calibration files matching whichwave, which has the following form:
;	files: String array of filenames.
;	waves: String array of wavelengths (should match whichwave).
;	cpols: Array of calibration polarizer states (should all be 1).
;	crets: Array of calibration retarder states (1 = in, 0 = out).
;	cangs: Array of calibration polarizer angles (note that there is a -1.4 
;			degree offset and a value of 0 means it's a flat with the polarizer removed).
;	mpols: String array, dimensions (nfiles, npchans), listing all the polarization analyzer
;			states contained in the file. If the file has fewer than npchans, the trailing entries
;			will be blank strings ('').
;	cvers: Array of instrument cover flags. Should all be 1 (i.e., no darks).
;	ctags: String array of tags uniquely specifying calibration optics configurations.
;			Consists of cpol, cret, and cang (rounded to nearest degree), space separated.
;
; Joseph Plowman
;-
function get_cal_info, files, npchans=npchans, whichwave=whichwave
	
	if(n_elements(npchans) eq 0) then npchans=6
	if(n_elements(wave) eq 0) then whichwave = 1075
	
	nfiles = n_elements(files)
	filesout = strarr(nfiles)
	waves=lonarr(nfiles) ; The central wavelength in the file, rounded to nearest nanometer
	cpols=lonarr(nfiles) ; Whether or not the polarizer flag is set
	crets=lonarr(nfiles) ; Whether or not the retarder was in
	cangs=fltarr(nfiles) ; Calibration polarizer angles
	cvers=lonarr(nfiles) ; Is this a dark?
	ctags=strarr(nfiles) ; String which uniquely identifies cal setting
	mpols=strarr(nfiles,npchans) ; Measured polarizations in each file
	count=0
	for i=0,nfiles-1 do begin
		fits_open,files[i],fcb
		inventory,fcb,beam,group,wave,pols,type,expose,cover,cal_pol,cal_ret,cal_ang=cal_ang
		fits_close,fcb
		if(cal_pol eq 0) then cal_ang = 0.0
		if(cal_pol and round(median(wave)) eq whichwave and cover) then begin
			waves[count]=round(median(wave))
			cpols[count]=cal_pol
			crets[count]=cal_ret
			cangs[count]=cal_ang
			cvers[count]=cover
			ctags[count] = strjoin(strtrim(string(round([cal_pol,cal_ret,cal_ang])),2),' ')
			upols=pols[uniq(pols,sort(pols))]
			mpols[count,0:n_elements(upols)-1]=upols
			filesout[count]=files[i]
			count++
		endif
	endfor
		
	return, {files:filesout[0:count-1], waves:waves[0:count-1], cpols:cpols[0:count-1], $
			crets:crets[0:count-1], cangs:cangs[0:count-1], mpols:mpols[0:count-1,*], cvers:cvers[0:count-1], $ 
			ctags:ctags[0:count-1]}

end

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


;+
; Function to create images which specify the response of each pixel to each
; component of the Stokes vector. Related to the Mueller matrix of the 
; polarization analyzer in this configuration, except that we only see 
; intensities (scalars) at the detector - it simply sums the intensities of 
; each component of the Stokes vector. There are therefore four such images 
; for any given configuration of the polarization analyzer; equivalently,
; there is a vector for each pixel location whose inner product with
; a Stokes vector gives the intensity which would be observed by the 
; detector. Inputs:
;
;	coefs: The coefficients, as computed by channel_response_coefficients.
;			There should be nstokes*n_basis of them, and they vary first
;			over the spatial basis coefficients, then over the stokes
;			coefficients; see the code below for usage.
;	xybasis: Basis specifying how the polarization response can vary spatially. See comp_cal_xybasis.
;
; Returns the set of four images described above.
;
; Joseph Plowman
;-
function get_response_images, coefs, xybasis

	nx = n_elements(xybasis[*,0,0])
	ny = n_elements(xybasis[0,*,0])
	n_basis = n_elements(xybasis[0,0,*])
	nstokes = round(n_elements(coefs)/n_basis)
	
	images = dblarr(nx,ny,nstokes)
	for i=0,nstokes-1 do for j=0,n_basis-1 do images[*,*,i]+=coefs[i*nstokes+j]*xybasis[*,*,j]

	return, images
	
end

;+
; Function which computes the Mueller matrix of a linear polarizer
; given its transmission and orientation angle. Inputs:
;	trans: The transmission.
;	angle: The angle, in degrees.
;
; Returns the 4x4 mueller matrix.
;
; Joseph Plowman
;-
function mueller_polarizer,trans,angle

	ang=angle*!dpi/180.d0  ;convert to radians
	c2=cos(2.d0*ang) & s2=sin(2.d0*ang)

	matrix=trans*[[1.d0,c2,s2,0.d0],$
		[c2,c2^2,c2*s2,0.d0],$
		[s2,s2*c2,s2^2,0.d0],$
		[0.d0,0.d0,0.d0,0.d0]]

	return,matrix
end

;+
; Function which computes the Mueller matrix of a retarder given
; its transmission, retardation, and orientation angles. Inputs:
;	trans: The transmission.
;	angle: The orientation angle, in degrees.
;	delta: The retardation, in degrees.
;
; Returns the 4x4 Mueller matrix of the retarder.
;
; Joseph Plowman
;-
function mueller_retarder,trans,angle,delta,incidence=incidence
	
	ang=angle*!dpi/180.d0  ;convert to radians
	del=delta*!dpi/180.d0

	if keyword_set(incidence) then del=del*(1.+(sin(incidence*!dpi/180.)^2)/5.)

	c2=cos(2.d0*ang) & s2=sin(2.d0*ang)

	matrix=trans*[[1.d0,0.d0,0.d0,0.d0],$
		[0.d0,c2^2+s2^2*cos(del),c2*s2*(1.-cos(del)),-s2*sin(del)],$
		[0.d0,c2*s2*(1.-cos(del)),s2^2+c2^2*cos(del),c2*sin(del)],$
		[0.d0,s2*sin(del),-c2*sin(del),cos(del)]]

	return,matrix
	
end

;+
; Function to compute the Stokes vector of the CoMP calibration optics
; (polarizer and retarder). Has the following inputs:
;	cret: Flag specifying if retarder is present (1 if so).
;	cang: The angle of the cal polarizer.
; Optional keyword parameters:
;	ret: The retardance, in degrees (default is 94.438).
;	r_ang: The retarder orientation, in degrees (default is 0.0).
;	stokes: Stokes vector of light entering the retarder (default is [1,0,0,0]).
;	ptrans: The transmission of the polarizer (default is 0.45).
;	rtrans: The transmission of the retarder (default is 0.99).
;	cpol: Flag specifying if the polarizer is present (1 if so).
;
; Returns the 4 element Stokes vector corresponding to the input parameters.
;
; Joseph Plowman
;-
function get_cal_stokes_vector,cret,cang,ret=ret,r_ang=r_ang,stokes=stokes,ptrans=ptrans,rtrans=rtrans,cpol=cpol

	if(n_elements(ptrans) eq 0) then ptrans=0.45d0      ; cal polarizer transmission.
	if(n_elements(rtrans) eq 0) then rtrans=0.99d0         ; cal retarder transmission.
	if(n_elements(stokes) eq 0) then stokes=[1.d0,0.,0.,0.] ; unpolarized input Stokes vector.
	if(n_elements(ret) eq 0) then ret=94.438d0       ; retardance of cal retarder.
	if(n_elements(r_ang) eq 0) then r_ang = 0.0 ; The angle of the retarder.

	; If the state of the polarizer is not specified, find it by looking at the
	; polarizer angle. All reported polarizer angles have an offset of -1.4 degrees,
	; so an actual polarizer angle of 0 degrees will be reported as -1.4. On the
	; other hand, if the polarizer is not in, its angle is recorded in the
	; data as zero, so cang=0 probably means that the polarizer is not in.
	if(n_elements(cpol) eq 0) then cpol = cang ne 0
		
	obs_out=stokes
	if(cpol eq 1 and cang ne 0) then begin
		cang+=1.4 ; Fix the -1.4 degree offset in the angles...
		obs_out = mueller_polarizer(ptrans,cang)#obs_out ; Apply the polarizer matrix.
		if(cret) then obs_out=mueller_retarder(rtrans,r_ang,ret)#obs_out ; Apply retarder matrix if cret=1.
	endif
	
	return,obs_out
	
end

;+
; Function to compute a CoMP image given an input Stokes vector and a set of fitted
; polarization analyzer Stokes sensitivity coefficients. Inputs:
;	coefs: Structure containing the upper and lower coefficients, which should contain:
;		uppercoefs: Array of coefficients for the upper (above the diagonal) part of the image,
;				as computed by channel_response_coefficients. See also get_response_images.
;		lowercoefs: Array of coefficients for the lower (below the diagonal) part of the image,
;				as computed by channel_response_coefficients. See also get_response_images.
;		xybasis: Basis specifying how the polarization response can vary spatially. See comp_cal_xybasis.
;		uppermask: Mask for the upper pixels (above the diagonal).
; 		lowermask: Mask for the lower pixels (below the diagonal).
;
;	stokes: An input stokes vector, or image of stokes vectors (dimensions [nx,ny,nstokes]).
;			If an image, must have same image dimensions as xybasis. 
;
;	Returns a modeled image of that polarization analyzer channel.
;
; Joseph Plowman
;-
function compute_cal_image, coefs, stokes
	
	; Create images which specify the response of each pixel to each component
	; of the Stokes vector. Related to the Mueller matrix of the polarization
	; analyzer in this configuration, except that we only see intensities
	; (scalars) at the detector - it simply sums the intensities of each component
	; of the Stokes vector.
	coefs_upper = get_response_images(coefs.uppercoefs, coefs.xybasis)
	coefs_lower = get_response_images(coefs.lowercoefs, coefs.xybasis)

	nx=n_elements(coefs_upper[*,0,0])
	ny=n_elements(coefs_upper[0,*,0])
	n_stokes=n_elements(coefs_upper[0,0,*])
	image=dblarr(nx,ny)
	
	if(size(reform(stokes),/n_dimensions) eq 1) then begin ; Stokes is a single vector:
		for i=0,n_stokes-1 do begin
			image += stokes[i]*(coefs_upper[*,*,i]*coefs.uppermask+coefs_lower[*,*,i]*coefs.lowermask)
		endfor
	endif else begin ; Stokes consists of a 2-dimensional image with a Stokes vector at each position:
		for i=0,n_stokes-1 do begin
			image += stokes[*,*,i]*(coefs_upper[*,*,i]*coefs.uppermask+coefs_lower[*,*,i]*coefs.lowermask)
		endfor
	endelse
	
	return, image
	
end 
