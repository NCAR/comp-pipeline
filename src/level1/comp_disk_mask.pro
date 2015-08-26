; docformat = 'rst'

;+
; Procedure to create occulter mask. If `dx` and `dy` are present, the mask
; will be shifted by `dx`, `dy` due to the image being off-center in the
; detector.
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
;-
function comp_disk_mask, radius, dx=dx, dy=dy
  compile_opt strictarr
  @comp_constants_common

  mask = fltarr(nx, ny) + 1.

  x = rebin(indgen(nx) - nx / 2., nx, nx)
  y = transpose(x)
  if (n_elements(dx) gt 0 or n_elements(dy) gt 0) then begin
    x -= dx
    y -= dy
  endif

  r = sqrt(x^2 + y^2)

  bad = where(r lt radius, count)
  if (count gt 0) then mask[bad] = 0.

  return, mask
end
