; docformat = 'rst'

;+
; Applies distortion correction to a sub-image `dat` and given the
; distortion coefficients.
;
; :Params:
;   dat : in, out, required, type=fltarr
;     subimage to correct
;   dx_c : in, required, type="fltarr(3, 3)"
;     x coefficients for subimage
;   dy_c : in, required, type="fltarr(3, 3)"
;     y coefficients for subimage
;-
function comp_apply_distortion, dat, dx_c, dy_c
  compile_opt strictarr

  s = size(dat)
  nx = s[1]
  ny = s[2]

  x = rebin(findgen(nx), nx, ny)
  y = transpose(x)

  dat1 = interpolate(dat, $
                     x + comp_eval_surf(dx_c, findgen(nx), findgen(ny)), $
                     y + comp_eval_surf(dy_c, findgen(nx), findgen(ny)), $
                     cubic=-0.5, missing=0.0)
end