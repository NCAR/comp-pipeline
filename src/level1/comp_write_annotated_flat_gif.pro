; docformat = 'rst'

;+
; Write an annotated GIF of a flat image.
;
; :Params:
;   image : in, required, type="fltarr(1024, 1024)"
;     flat image to use as base of plot
;   occulter1 : in, required, type=structure
;     image geometry structure describing occulter for sub-image 1
;   occulter2 : in, required, type=structure
;     image geometry structure describing occulter for sub-image 2
;   field1 : in, required, type=structure
;     image geometry structure describing field for sub-image 1
;   field2 : in, required, type=structure
;     image geometry structure describing field for sub-image 2
;
; :Keywords:
;   filename : in, required, type=string
;     filename for output GIF
;-
pro comp_write_annotated_flat_gif, image, occulter1, occulter2, field1, field2, $
                                   filename=filename
  compile_opt strictarr

  ; convert 620x620 geometry to 1024x1024 geometry

  fullsize_occulter1 = occulter1
  fullsize_occulter2 = occulter2
  fullsize_field1 = field1
  fullsize_field2 = field2

  fullsize_occulter1.y += 1024 - 620
  fullsize_occulter2.x += 1024 - 620
  fullsize_field1.y += 1024 - 620
  fullsize_field2.x += 1024 - 620

  dims = size(image, /dimensions)

  ; setup graphics

  original_device = !d.name
  set_plot, 'Z'
  device, get_decomposed=original_decomposed
  device, set_resolution=dims, z_buffering=0, decomposed=0
  loadct, 0, /silent, n_colors=254

  occulter_color = 254
  tvlct, 255, 255, 0, occulter_color

  field_color = 255
  tvlct, 0, 255, 0, field_color

  t = findgen(361) * !dtor

  ; start displaying

  tv, bytscl(im, min=0.0, max=200.0)   ; scale 0.0 - 200.0 seems OK

  x = fullsize_occulter1.radius * cos(t) + fullsize_occulter1.x
  y = fullsize_occulter1.radius * cos(t) + fullsize_occulter1.y
  plots, x, y, /device, color=occulter_color
  plots, [fullsize_occulter1.x, fullsize_occulter1.y], $
         /device, color=occulter_color, psym=1

  x = fullsize_field1.radius * cos(t) + fullsize_field1.x
  y = fullsize_field1.radius * cos(t) + fullsize_field1.y
  plots, x, y, /device, color=occulter_color
  plots, [fullsize_field1.x, fullsize_field1.y], $
         /device, color=field_color, psym=1

  x = fullsize_occulter2.radius * cos(t) + fullsize_occulter2.x
  y = fullsize_occulter2.radius * cos(t) + fullsize_occulter2.y
  plots, x, y, /device, color=occulter_color
  plots, [fullsize_occulter2.x, fullsize_occulter2.y], $
         /device, color=occulter_color, psym=1

  x = fullsize_field2.radius * cos(t) + fullsize_field2.x
  y = fullsize_field2.radius * cos(t) + fullsize_field2.y
  plots, x, y, /device, color=occulter_color
  plots, [fullsize_field2.x, fullsize_field2.y], $
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
