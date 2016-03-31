; docformat = 'rst'

;+
;
; :Returns:
;
; :Params:
;    image
;    backgnd
;    llx
;    lly
;
; :Author:
;   Christian Bethge
;-
function comp_transparent_logo, image, backgnd, llx, lly
  compile_opt strictarr

  alpha_channel = image[*, *, 3]
  scaled_alpha = float(alpha_channel) / float(max(alpha_channel))
  s = size(image[*, *, 0:2], /dimensions)
  alpha = rebin(scaled_alpha, s[0], s[1], s[2])
  foregnd = image[*, *, 0:2]
  final_image = foregnd * alpha + (1 - alpha) * backgnd

  return, final_image
end
