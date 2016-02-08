; docformat = 'rst'

;+
; Function to compute the Stokes vector of the CoMP calibration optics
; (polarizer and retarder).
;
; :Returns:
;   the 4 element Stokes vector corresponding to the input parameters
;
; :Params:
;   cret : in, type=boolean
;     flag specifying if retarder is present (1 if so)
;  cang :
;    The angle of the cal polarizer.
;
; :Keywords:
;   ret : in, optional, type=boolean, default=94.438
;     the retardance, in degrees
;   r_ang : in, optional, type=double, default=0.0
;     the retarder orientation, in degrees
;   stokes : in, optional, type=dblarr(4), default="[1, 0, 0, 0]"
;     Stokes vector of light entering the retarder
;   ptrans : in, optional, type=double, default=0.45
;     the transmission of the polarizer
;   rtrans : in, optional, type=double, default=0.99
;    the transmission of the retarder
;   cpol : in, optional, type=boolean
;     flag specifying if the polarizer is present (1 if so)
;
; :Author:
;   Joseph Plowman
;-
function comp_get_cal_stokes_vector, cret, cang, $
                                     ret=ret, $
                                     r_ang=r_ang, $
                                     stokes=stokes, $
                                     ptrans=ptrans, $
                                     rtrans=rtrans, $
                                     cpol=cpol
  compile_opt strictarr

  if (n_elements(ptrans) eq 0) then ptrans = 0.45D   ; cal polarizer transmission.
  if (n_elements(rtrans) eq 0) then rtrans = 0.99D   ; cal retarder transmission.
  if (n_elements(stokes) eq 0) then stokes = [1.0D, 0.0D, 0.0D, 0.0D] ; unpolarized input Stokes vector
  if (n_elements(ret) eq 0) then ret = 94.438D       ; retardance of cal retarder
  if (n_elements(r_ang) eq 0) then r_ang = 0.0       ; the angle of the retarder

  ; If the state of the polarizer is not specified, find it by looking at the
  ; polarizer angle. All reported polarizer angles have an offset of -1.4
  ; degrees, so an actual polarizer angle of 0 degrees will be reported as
  ; -1.4. On the other hand, if the polarizer is not in, its angle is recorded
  ; in the data as zero, so cang=0 probably means that the polarizer is not in.
  if (n_elements(cpol) eq 0) then cpol = cang ne 0

  obs_out = stokes
  if (cpol eq 1 and cang ne 0) then begin
    cang += 1.4   ; fix the -1.4 degree offset in the angles...
    ; apply the polarizer matrix
    obs_out = comp_mueller_polarizer(ptrans, cang) # obs_out
    ; apply retarder matrix if cret=1
    if (cret) then obs_out = comp_mueller_retarder(rtrans, r_ang, ret) # obs_out
  endif

  return, obs_out
end
