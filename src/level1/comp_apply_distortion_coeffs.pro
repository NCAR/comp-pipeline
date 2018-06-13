; docformat = 'rst'

;+
; Applies distortion correction to a sub-image `sub_image` and given the
; distortion coefficients.
;
; :Params:
;   sub_image : in, out, required, type="fltarr(nx, ny)"
;     sub-image to correct
;   k : in, required, type=float
;     distortion coefficient for subimage
;
; :Author:
;   MLSO Software Team
;-
function comp_apply_distortion_coeffs, sub_image, k
  compile_opt strictarr

  dims = size(sub_image, /dimensions)
  nx = dims[0]
  ny = dims[1]

  x = dindgen(nx, ny) mod nx
  y = transpose(dindgen(ny, nx) mod ny)

  x_new = x * 0.5 * (1.0 + k) + y * 0.5 * (1.0 - k)
  y_new = x * 0.5 * (1.0 - k) + y * 0.5 * (1.0 + k)

  dist_corrected = interpolate(sub_image, x_new, y_new, $
                               cubic=-0.5, missing=0.0)
  return, dist_corrected
end
