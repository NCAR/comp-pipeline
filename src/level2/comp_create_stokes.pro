; docformat = 'rst'

;+
; Procedure to create simulated Stokes profiles of coronal emission lines.
;
; :Params:
;   stokes_i : out, optional
;   stokes_q : out, optional
;   stokes_u : out, optional
;   stokes_v : out, optional
;
; :Author:
;   MLSO Software Team
;-
pro comp_create_stokes, stokes_i, stokes_q, stokes_u, stokes_v
  compile_opt strictarr
  @comp_simulate_common

  ; gaussian shape assumed for emission line

  ; Note: the sign of Stokes V is positive in the blue lobe for positive
  ; field for an absorption line, and has the opposite sign for an emission
  ; line. This routine assumes an emission line

  cv = -8.09e-6   ; Stokes V calibration factor
  
  c = 3e5
  
  ; convert line width to wavelength
  dop_wav2 = (wavelength * line_width / c) ^ 2.0
  ; convert velocity to wavelength
  vel_wav = wavelength * velocity / c
  lwv = lambda - wavelength - vel_wav
  
  stokes_i = intensity * exp(-1.0 * lwv ^ 2.0 / dop_wav2)
  stokes_q = stokes_i * linear * cos(azimuth * !pi / 90.0)
  stokes_u = stokes_i * linear * sin(azimuth * !pi / 90.0)
  ; below is cv * bfield * dstokes_i / dlambda
  stokes_v = -2.0 * cv * bfield * lwv / dop_wav2 * stokes_i
end
