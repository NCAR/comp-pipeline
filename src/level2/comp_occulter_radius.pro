; docformat = 'rst'

;+
; Returns size in pixels for a given occulter ID.
;
; :Returns:
;   float
;
; :Params:
;   occulter_id : in, required, type=long
;     occulter ID as reported by OCCULTER in the FITS header
;-
function comp_occulter_radius, occulter_id
  compile_opt strictarr
  on_error, 2

  ; for 2017
  ; id=27 [21.7170 mm]: mean 226.162, median 226.217
  ; id=31 [22.0220 mm]: mean 229.071, median 229.106
  ; id=35 [22.2250 mm]: mean 232.339, median 232.300

  ; fitting a quadratic through size in mm to median radius in pixels gives
  ; coefficients:
  coeffs = [5915.57, -529.667, 12.3263]

  return, poly(comp_occulter_id(occulter_id), coeffs)
end

