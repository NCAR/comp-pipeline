; docformat = 'rst'

;+
; Procedure to compute analytic fit to a gaussian sampled at three points.
;
; :Params:
;   profile : in, required, type=fltarr(3)
;     the intensity at three points in the line profile increasing
;     monotonically in wavelength
;   d_lambda : in, required, type=float
;     the wavelegth spacing of the samples
;   doppler_shift : out, optional, type=float
;     the shift of the fit gaussian from the center wavelength in the same
;     units as `d_lambda`
;   width : out, optional, type=float
;     the linewidth in the same units as `d_lambda`
;   i_cent : out, optional, type=float
;     the central intensity of the gaussian in the same units as `profile`
;
; :Author:
;   MLSO Software Team
;
; :History:
;   Christian Bethge
;   see git log for recent changes
;-
pro comp_analytic_gauss_fit, profile, d_lambda, doppler_shift, width, i_cent
  compile_opt strictarr

  if (profile[0] lt 0 or profile[1] lt 0 or profile[2] lt 0) then begin
  ; Removing restrictions on wings temporarily...
  ;if (profile[1] lt 0) then begin
    width = 0D
    doppler_shift = 0D
    i_cent = 0D
    goto, skip_calc
  endif

  i1 = profile[0]
  i2 = profile[1]
  i3 = profile[2]

  a = alog(i3 / i2)
  b = alog(i1 / i2)

  if ((-2D * d_lambda^2D / (a + b)) lt 0) then begin
    width = 0D
    doppler_shift = 0D
    i_cent = 0D
    goto, skip_calc
  endif

  width = sqrt(-2D * d_lambda^2D / (a + b))
  doppler_shift = width^2D/(4D * d_lambda) * (a - b)
  i_cent = i2 * exp(doppler_shift^2D / width^2D)

  skip_calc:
end
