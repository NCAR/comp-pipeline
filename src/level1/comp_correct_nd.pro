; docformat = 'rst'

;+
; Return the transmission correction for an ND filter at a given wavelength.
;
; :Returns:
;   float
;
; :Params:
;   nd : in, required, type=long
;     ND value for the image
;   wavelength : in, required, type=float
;     wavelength corresponding to image
;-
function comp_correct_nd_transmission, nd, wavelength
  compile_opt strictarr

  ; currently, we use the same transmissions for all wavelengths because all of
  ; CoMP's wavelengths are very close; transmissions will vary by wavelength
  ; in UCoMP
  transmissions = [, , , , , , , 1.0]

  return, transmissions[nd - 1]
end


;+
; Find correction for ND filter given the filters used on the image and its
; flat.
;
; :Returns:
;   float
;
; :Params:
;   nd : in, required, type=long
;     ND value for the image
;   flat_nd : in, required, type=long
;     ND value for the flat image
;   wavelength : in, required, type=float
;     wavelength corresponding to image
;-
function comp_correct_nd, nd, flat_nd, wavelength
  compile_opt strictarr

  if (nd eq flat_nd) then return, 1.0

  image_transmission = comp_correct_nd_transmission(nd, wavelength)
  flat_transmission = comp_correct_nd_transmission(flat_nd, wavelength)

  return, image_transmission / flat_transmission
end
