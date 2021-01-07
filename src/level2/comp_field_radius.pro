; docformat = 'rst'

;+
; Radius of field in pixels.
;
; :Returns:
;   float
;-
function comp_field_radius
  compile_opt strictarr

  ; for 2017:
  ; field: mean 299.319, median 299.348

  return, 299.348
end
