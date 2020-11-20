; docformat = 'rst'

;+
; Create a GIF of a raw CoMP FITS file.
;
; :Params:
;   l0_filename : in, required, type=string
;     filename of raw CoMP FITS file
;   output_filename : in, required, type=string
;     filename of output FITS file
;
; :Keywords:
;   extension : in, optional, type=integer, default=0
;     extension of `l0_filename` to display
;-
pro comp_make_l0_gif, l0_filename, output_filename, $
                      extension=extension, $
                      annotation_text=annotation_text, $
                      display_minimum=display_minimum, $
                      display_maximum=display_maximum
  compile_opt strictarr

  _extension = n_elements(extension) eq 0L ? 1L : extension

  fits_open, l0_filename, fcb
  fits_read, fcb, data, header, exten_no=_extension
  fits_close, fcb

  dims = size(data, /dimensions)

  _display_min = mg_default(display_min, 2000.0)
  _display_max = mg_default(display_max, 10000.0)

  n_annotation_colors = 1L
  top = 255L - n_annotation_colors

  ; save original graphics settings
  original_device = !d.name

  ; setup graphics device
  set_plot, 'Z'

  device, get_decomposed=original_decomposed
  tvlct, original_rgb, /get

  device, set_resolution=dims, $
          z_buffering=0, $
          decomposed=0, $
          set_pixel_depth=8, $
          set_colors=256

  loadct, 0, /silent, ncolors=top + 1L
  white = 255
  tvlct, 255, 255, 255, white
  tvlct, r, g, b, /get

  ; display image
  tv, bytscl(data, min=_display_min, max=_display_max, top=top)

  ; annotation
  line_height = 0.02
  top = 0.97
  xyouts, 0.975, top, /normal, alignment=1.0, $
          file_basename(l0_filename), color=white
  if (n_elements(annotation_text) gt 0L) then begin
    xyouts, 0.975, top - line_height, /normal, alignment=1.0, $
            annotation_text, color=white
  endif

  ; save image to output
  write_gif, output_filename, tvrd(), r, g, b

  ; restore original graphics settings
  device, decomposed=original_decomposed
  tvlct, original_rgb
  set_plot, original_device
end
