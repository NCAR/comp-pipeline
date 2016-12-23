; docformat = 'rst'

;+
; Applies distortion correction to the sub-images `dat1` and `dat2` given the
; distortion coefficients.
;
; :Params:
;   dat1 : in, out, required, type=fltarr
;     upper-left subimage to correct
;   dat2 : in, out, required, type=fltarr
;     lower-right subimage to correct
;   dx1_c : in, required, type="fltarr(3, 3)"
;     x coefficients for upper-left subimage
;   dy1_c : in, required, type="fltarr(3, 3)"
;     y coefficients for upper-left subimage
;   dx2_c : in, required, type="fltarr(3, 3)"
;     x coefficients for lower-right subimage
;   dy2_c : in, required, type="fltarr(3, 3)"
;     y coefficients for lower-right subimage
;-
pro comp_apply_distortion, dat1, dat2, dx1_c, dy1_c, dx2_c, dy2_c
  compile_opt strictarr

  s = size(dat1)
  nx = s[1]

  x = rebin(findgen(nx), nx, nx)
  y = transpose(x)

  dat1 = interpolate(dat1, $
                     x + comp_eval_surf(dx1_c, x, y), $
                     y + comp_eval_surf(dy1_c, x, y), $
                     cubic=-0.5, missing=0.0)
  dat2 = interpolate(dat2, $
                     x + comp_eval_surf(dx2_c, x, y), $
                     y + comp_eval_surf(dy2_c, x, y), $
                     cubic=-0.5, missing=0.0)
end