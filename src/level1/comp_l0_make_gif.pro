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
pro comp_l0_make_gif, l0_filename, output_filename, extension=extension
  compile_opt strictarr

  _extension = n_elements(extension) eq 0L ? 1L : extension

  fits_open, l0_filename, fcb
  fits_read, fcb, data, header, exten_no=_extension
  fits_close, fcb

  xsize = 1024L
  ysize = 1024L

  display_min = 2000.0
  display_max = 10000.0

  n_annotation_colors = 1L
  top = 255L - n_annotation_colors

  ; save original graphics settings
  original_device = !d.name
  device, get_decomposed=original_decomposed
  tvlct, original_rgb, /get

  ; setup graphics device
  set_plot, 'Z'
  device, set_resolution=[xsize, ysize], $
          z_buffering=0, $
          decomposed=0, $
          set_pixel_depth=8, $
          set_colors=256

  loadct, 0, /silent, ncolors=top + 1L
  white = 255
  tvlct, 255, 255, 255, white
  tvlct, r, g, b, /get

  ; display image
  tv, bytscl(data, min=display_min, max=display_max, top=top)

  ; annotation
  xyouts, 0.05, 0.95, /normal, $
          file_basename(l0_filename), color=white

  ; save image to output
  write_gif, output_filename, tvrd(), r, g, b

  ; restore original graphics settings
  set_plot, original_device
  device, decomposed=original_decomposed
  tvlct, original_rgb
end
