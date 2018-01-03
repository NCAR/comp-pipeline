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

default, xcen, 309.5
default, ycen, 309.5


  compile_opt strictarr
  @comp_constants_common

  mask = fltarr(nx, ny) + 1.0

     x = findgen(nx,ny)mod(nx) - xcen
     y = transpose(findgen(ny,nx)mod(ny) ) - ycen
     x=double(x)
     y=double(y)

  r = sqrt(x^2 + y^2)

  bad = where(r gt radius, count)
  if (count gt 0) then mask[bad] = 0.0

  return, mask
end
