; docformat = 'rst'

;+
; :Params:
;   dir1 : in, required, type=string
;   dir2 : in, required, type=string
;   basename : in, required, type=string
;   ext : in, required, type=integer
;-
pro comp_compare_images, dir1, dir2, basename, ext
  compile_opt strictarr

  filename1 = filepath(basename, root=dir1)
  filename2 = filepath(basename, root=dir2)
  fits_open, filename1, fcb1
  fits_open, filename2, fcb2
  fits_read, fcb1, image1, header1, exten_no=ext
  fits_read, fcb2, image2, header2, exten_no=ext
  fits_close, fcb1
  fits_close, fcb2

  ; display images, difference
  dims = size(image1, /dimensions)
  window, xsize=3 * dims[0], ysize=dims[1], /free, title=basename
  tvscl, image1, 0
  tvscl, image2, 1
  diff = image1 - image2
  r = mg_range(diff)
  tv, diff, 2

  ; TODO: display stats
end


; main-level example

; display a few Q and U line-center images from the .crosstalk-currentvalues
; and .crosstalk-20160725values runs

end
