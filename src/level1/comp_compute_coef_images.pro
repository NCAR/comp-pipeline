; docformat = 'rst'

;+
; Compute calibration coefficient images, as used by comp_compute_calibrated_stokes.
; This assumes we want the full 4-dimensional Stokes vector - modifying to do the
; reduced rank inversion is left as an exercise...
;
; :Returns:
;   Returns an array of calibration coefficients which can be used to calibrate
;   and demodulate the data array. Note that the array will have the same
;   dimensions as the arrays in the calibration structure.
;
; :Params:
;   cal_struct : in, required, type=structure
;     Structure containing calibration information, as produced by
;     `comp_cal_example_script.pro`
;   data_labels : in, required, type=strarr
;     Array of labels of the polarization states in the data array which is to
;     be calibrated.
;
; :Author:
;   Joseph Plowman
;-
function comp_compute_coef_images, cal_struct, data_labels
  compile_opt strictarr

  nstokes     = 4
  nx          = n_elements(cal_struct.mask[*, 0])
  ny          = n_elements(cal_struct.mask[0, *])
  n_basis     = n_elements(cal_struct.xybasis[0, 0, *])
  nlabels     = n_elements(data_labels)
  coef_images = dblarr(nx, ny, nlabels, nstokes)

  for i = 0, nlabels - 1 do begin
    ical = where(data_labels[i] eq cal_struct.upols)
    for j = 0, nstokes - 1 do begin
      for k = 0, n_basis - 1 do begin
        upper = cal_struct.uppercoefs[ical, j * n_basis + k] * cal_struct.uppermask
        lower = cal_struct.lowercoefs[ical, j * n_basis + k] * cal_struct.lowermask
        coef_images[*, *, i, j] += (upper + lower) * cal_struct.mask * cal_struct.xybasis[*, *, k]
      endfor
    endfor
  endfor

  return, coef_images
end
