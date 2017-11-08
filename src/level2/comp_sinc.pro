; docformat = 'rst'

;+
; Function to calculate approximate comp transmission profile.
;
; a four stage filter with a free spectral range of 2.30 nm is assumed
; the FSR of the CoMP filter is 2.30 nm at 1074.6 nm
;
; wave is desired central wavelength (nm)
; lambda is wavelength scale (nm)
;
; note: maximum transmission is normalized to unity
;
; :Author:
;   MLSO Software Team
;-
function comp_sinc, lambda, wave
  compile_opt strictarr

  return, sinc((wave - lambda) / (2.30 / 2^4))^2
end