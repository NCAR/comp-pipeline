; docformat = 'rst'

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
function comp_get_response_images, coefs, xybasis

	nx = n_elements(xybasis[*,0,0])
	ny = n_elements(xybasis[0,*,0])
	n_basis = n_elements(xybasis[0,0,*])
	nstokes = round(n_elements(coefs)/n_basis)

	images = dblarr(nx,ny,nstokes)
	for i=0,nstokes-1 do for j=0,n_basis-1 do images[*,*,i]+=coefs[i*nstokes+j]*xybasis[*,*,j]

	return, images

end

