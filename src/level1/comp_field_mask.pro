; docformat = 'rst'

;+
; Procedure to create field mask. If xcen and ycen are not specified, the
; code assumes the image is 620x620 and the occulter is already shifted to
; the middle of the array
;
; :Uses:
;   comp_constants_common
;
; :Returns:
;   mask
;
; :Params:
;   radius : in, required, type=float
;
; :Keywords:
;   xcen : in, optional, type=float
;     x-coordinate of occulter center 
;   ycen : in, optional, type=float
;     y-coordinate of occulter center 
;
; :Author:
;   MLSO Software Team
;-
function comp_field_mask, radius, xcen=xcen, ycen=ycen
  compile_opt strictarr
  @comp_constants_common

  default, xcen, 309.5
  default, ycen, 309.5

  mask = fltarr(nx, ny) + 1.0

  x = dindgen(nx, ny) mod nx - xcen
  y = transpose(dindgen(ny, nx) mod ny ) - ycen

  r = sqrt(x^2 + y^2)

  bad = where(r gt radius, count)
  if (count gt 0) then mask[bad] = 0.0

  return, mask
end
