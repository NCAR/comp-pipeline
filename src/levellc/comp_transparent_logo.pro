; docformat = 'rst'

;+
; Blend a partially transparent logo into a background image.
;
; :Returns:
;   `arr(*, *, 3)`
;
; :Params:
;   image : in, required, type="arr(*, *, 4)"
;     logo image
;   background : in, required, type="arr(*, *, 3)"
;     background image
;
; :Author:
;   Christian Bethge
;-
function comp_transparent_logo, image, background, x, y
  compile_opt strictarr

  alpha_channel = image[*, *, 3]
  scaled_alpha = float(alpha_channel) / float(max(alpha_channel))

  dims = size(image[*, *, 0:2], /dimensions)
  alpha = rebin(scaled_alpha, dims[0], dims[1], dims[2])   ; fill out 3rd dim

  foreground = image[*, *, 0:2]

  return, foreground * alpha + (1 - alpha) * background
end
