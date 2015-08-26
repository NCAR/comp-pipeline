; docformat = 'rst'

;+
; :Returns:
;   `fltarr(nx, nx)`
;
; :Params:
;   angle : in, required
;   post_width : in, required
;-
function comp_post_mask, angle, post_width
  compile_opt idl2
  @comp_constants_common
  @comp_mask_constants_common

  post_mask = fltarr(nx, nx) + 1.

  x = rebin(indgen(nx) - nx / 2., nx, nx)
  y = transpose(x)

  ;post_mask[where(abs(x) lt post_width/2. and y lt 0.)]=0.   ;mask out occulter post (to south)

  ; mask out occulter post (to north)
  post_mask[where(abs(x) lt post_width / 2. and y gt 0.)] = 0.

  ;post_mask[where(abs(y) lt post_width/2. and x gt 0.)]=0.   ;mask out occulter post at angle 0

  ;  post_mask=rot(post_mask, angle + post_rotation, /interp) ; small correction for post location

  ; negate because positive rot is clockwise, opposite of position angle
  post_mask = rot(post_mask, -angle, /interp)

  ; remask where rotate made values between 0 and 1
  bad = where(post_mask gt 0 and post_mask lt 1, count)
  if (count gt 0) then begin
    post_mask[bad] = 1.0
  endif

  return, post_mask
end
