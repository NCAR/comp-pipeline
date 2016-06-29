; docformat = 'rst'

;+
; Fix Rockwell.
;
; :Returns:
;   fixed raw image, `fltarr(1024, 1024)`
;
; :Params:
;   img : in, required, type="fltarr(1024, 1024)"
;     raw image
;   scale : in, required, type=float
;     normalization scale for derivative

; :History:
;   used compound assignment operators to save memory.   Oct 3 2013 GdT 
;-
function comp_fixrock, img, scale
  compile_opt strictarr

  ; simple derivative - pixel to pixel change left to right

  xderr = img - shift(img, 1, 0)

  ; scale down
  xderr *= scale

  ; subtract off the derivative image shifted modulo 256 pixels in X
  fred = img
  fred -= shift(xderr, 0,   0)
  fred -= shift(xderr, 256, 0)
  fred -= shift(xderr, 512, 0)
  fred -= shift(xderr, 768, 0)

  ; flip derivative to the other vertical half of the detector
  xderr = reverse(xderr, 2)

  ; subtract off the derivative image shifted modulo 256 pixels in X
  fred -= shift(xderr, 0,   0)
  fred -= shift(xderr, 256, 0)
  fred -= shift(xderr, 512, 0)
  fred -= shift(xderr, 768, 0)

  return, fred
end
