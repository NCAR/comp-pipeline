; docformat = 'rst'

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
function comp_get_cal_stokes_vector,cret,cang,ret=ret,r_ang=r_ang,stokes=stokes,ptrans=ptrans,rtrans=rtrans,cpol=cpol

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
		obs_out = comp_mueller_polarizer(ptrans,cang)#obs_out ; Apply the polarizer matrix.
		if(cret) then obs_out=comp_mueller_retarder(rtrans,r_ang,ret)#obs_out ; Apply retarder matrix if cret=1.
	endif

	return,obs_out

end
