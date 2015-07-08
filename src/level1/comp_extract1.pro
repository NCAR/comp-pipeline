; docformat = 'rst'

;+
; Procedure to extract sub-image from comp data. Image is extracted from the
; upper left corner and will have size `nx` which can be input.
;
; :Uses:
;   comp_constants_common
;
; :Returns:
;   subset of `data`
;
; :Params:
;   data : in, required, type=2D array
;     CoMP data
;-
function comp_extract1, data
  compile_opt strictarr
  @comp_constants_common

  return, data[0L:nx - 1L, 1024L - nx:1024L - 1L]
end
