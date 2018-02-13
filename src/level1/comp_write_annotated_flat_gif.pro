; docformat = 'rst'

;+
; Write an annotated GIF of a flat image.
;
; :Params:
;   flat : in, required, type="fltarr(620, 620)"
;     distortion corrected flat image to use as base of plot
;   occulter : in, required, type=structure
;     image geometry structure describing occulter for sub-image 
;   field : in, required, type=structure
;     image geometry structure describing field for sub-image
;
; :Keywords:
;   filename : in, required, type=string
;     filename for output GIF
;-
pro comp_write_annotated_flat_gif, flat, occulter, field, filename=filename
  compile_opt strictarr

  dims = size(flat, /dimensions)

  ; setup graphics

  original_device = !d.name
  set_plot, 'Z'
  device, get_decomposed=original_decomposed
  device, set_resolution=dims, z_buffering=0, decomposed=0
  loadct, 0, /silent, ncolors=254

  occulter_color = 254
  tvlct, 255, 255, 0, occulter_color

  field_color = 255
  tvlct, 0, 255, 0, field_color

  t = findgen(361) * !dtor

  ; start displaying

  tv, bytscl(flat, top=253)   ; scale 0.0 - 200.0 seems OK

  x = occulter.r * cos(t) + occulter.x
  y = occulter.r * sin(t) + occulter.y
  plots, x, y, /device, color=occulter_color
  plots, [occulter.x, occulter.y], $
         /device, color=occulter_color, psym=1

  x = field.r * cos(t) + field.x
  y = field.r * sin(t) + field.y
  plots, x, y, /device, color=field_color
  plots, [field.x, field.y], $
         /device, color=field_color, psym=1

  ; save results

  im = tvrd()
  tvlct, r, g, b, /get
  write_gif, filename, im, r, g, b

  ; restore graphics to original state

  done:
  device, decomposed=original_decomposed
  set_plot, original_device
end
