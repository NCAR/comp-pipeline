; docformat = 'rst'

;+
; Mask the post.
;
; :Uses:
;   comp_constants_common, comp_mask_constants_common
;
; :Returns:
;   `fltarr(nx, nx)`
;
; :Params:
;   angle : in, required, type=float
;     angle of post
;   post_width : in, required, type=float
;     width of post in pixels
;
; :Author:
;   MLSO Software Team
;-
function comp_post_mask, angle, post_width
  compile_opt idl2
  @comp_constants_common

  post_mask = fltarr(nx, ny) + 1.0

  x = findgen(nx,ny)mod(nx) -  nx*0.5 + 0.5
  y = transpose(findgen(ny,nx)mod(ny) ) - ny*0.5 + 0.5

  ;mask out occulter post (to south)
  ;post_mask[where(abs(x) lt post_width/2. and y lt 0.)]=0.

  ; mask out occulter post (to north)
  post_mask[where(abs(x) lt post_width*0.5 and y gt 0.)] = 0.

  ;mask out occulter post at angle 0
  ;post_mask[where(abs(y) lt post_width/2. and x gt 0.)]=0.

  ; small correction for post location
  ;  post_mask=rot(post_mask, angle + post_rotation, /interp)

  ; negate because positive rot is clockwise, opposite of position angle
  post_mask = rot(post_mask, -angle, /interp)

  ; remask where rotate made values between 0 and 1
  bad = where(post_mask gt 0 and post_mask lt 1, count)
  if (count gt 0) then begin
    post_mask[bad] = 1.0
  endif

  return, post_mask
end
