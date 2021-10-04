; docformat = 'rst'

;+
; Procedure to compute analytic fit to a gaussian sampled at three points.
;
; :Params:
;   i1 : in, required
;     the intensity at one of three points in the line profile increasing
;     monotonically in wavelength
;   i2 : in, required
;     the intensity at one of three points in the line profile increasing
;     monotonically in wavelength
;   i3 : in, required
;     the intensity at one of three points in the line profile increasing
;     monotonically in wavelength
;   d_lambda : in, required, type=float
;     the wavelegth spacing of the samples
;   doppler_shift : out
;     the shift of the fit gaussian from the center wavelength in the same
;     units as d_lambda
;   width : out
;     the linewidth in the same units as d_lambda
;   i_cent : out, optional, type=fltarr
;     the central intensity of the gaussian in the same units as i1, i2, i3
;
; :Author:
;   MLSO Software Team
;-
pro comp_analytic_gauss_fit2, i1, i2, i3, d_lambda, doppler_shift, width, i_cent
  compile_opt strictarr

  a = alog(i3 / i2)
  b = alog(i1 / i2)

  width = sqrt(-2.0 * d_lambda^2 / (a + b))
  doppler_shift = width^2 / (4.0 * d_lambda) * (a - b)
  i_cent = i2 * exp(doppler_shift^2 / width^2)

  i_cent[where(finite(i_cent, /nan))] = 0.0
  doppler_shift[where(finite(doppler_shift, /nan))] = 0.0
end
