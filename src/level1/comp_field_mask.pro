; docformat = 'rst'

;+
; Procedure to create field mask. If `dx` and `dy` are present, the mask will
; be shifted by `dx`, `dy` due to the field stop being off-center in the
; detector with respect to the occulter changed x, y equation to save memory
; (GdT).
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
;   dx : in, optional, type=float
;     shift in the x-direction
;   dy : in, optional, type=float
;     shift in the y-direction
;
; :Author:
;   MLSO Software Team
;-
function comp_field_mask, radius, dx=dx, dy=dy
  compile_opt strictarr
  @comp_constants_common

  mask = fltarr(nx, ny) + 1.0

  x = rebin(indgen(nx) - nx / 2.0, nx, ny)
  y = transpose(x)
  if (n_elements(dx) gt 0 or n_elements(dy) gt 0) then begin
    x -= dx
    y -= dy
  endif

  r = sqrt(x^2 + y^2)

  bad = where(r gt radius, count)
  if (count gt 0) then mask[bad] = 0.0

  return, mask
end
