; docformat = 'rst'

;+
; Function to compute a CoMP image given an input Stokes vector and a set of fitted
; polarization analyzer Stokes sensitivity coefficients. Inputs:
;	coefs: Structure containing the upper and lower coefficients, which should contain:
;		uppercoefs: Array of coefficients for the upper (above the diagonal) part of the image,
;				as computed by channel_response_coefficients. See also comp_get_response_images.
;		lowercoefs: Array of coefficients for the lower (below the diagonal) part of the image,
;				as computed by channel_response_coefficients. See also comp_get_response_images.
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
function comp_compute_cal_image, coefs, stokes

	; Create images which specify the response of each pixel to each component
	; of the Stokes vector. Related to the Mueller matrix of the polarization
	; analyzer in this configuration, except that we only see intensities
	; (scalars) at the detector - it simply sums the intensities of each component
	; of the Stokes vector.
	coefs_upper = comp_get_response_images(coefs.uppercoefs, coefs.xybasis)
	coefs_lower = comp_get_response_images(coefs.lowercoefs, coefs.xybasis)

	nx=n_elements(coefs_upper[*,0,0])
	ny=n_elements(coefs_upper[0,*,0])
	nstokes=n_elements(coefs_upper[0,0,*])
	image=dblarr(nx,ny)

	if(size(reform(stokes),/n_dimensions) eq 1) then begin ; Stokes is a single vector:
		for i=0,nstokes-1 do begin
			image += stokes[i]*(coefs_upper[*,*,i]*coefs.uppermask+coefs_lower[*,*,i]*coefs.lowermask)
		endfor
	endif else begin ; Stokes consists of a 2-dimensional image with a Stokes vector at each position:
		for i=0,nstokes-1 do begin
			image += stokes[*,*,i]*(coefs_upper[*,*,i]*coefs.uppermask+coefs_lower[*,*,i]*coefs.lowermask)
		endfor
	endelse

	return, image

end
