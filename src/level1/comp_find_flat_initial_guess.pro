; docformat = 'rst'

;+
; Find an initial guess for the center, as an offset from the center of the
; image.
;
; :Returns:
;   fltarr(2)
;
; :Params:
;   flat : in, required, type="fltarr(620, 620)"
;     flat to find center for, assumed to be dark corrected, hot pixel
;     corrected, etc.
;
; :Author:
;   MLSO Software Team
;-
function comp_find_flat_initial_guess, flat
  compile_opt strictarr
  @comp_constants_common

  ; threshold of 250.0 works well for 250.0 ms data for 1074 nm and 1079 nm
  ; in 2012 and later
  ; threshold = 250.0

  threshold = 20.0 * median(flat)

  ; mask flat into various in/out of annuli connected regions
  lr = label_region(flat lt threshold)

  ; assume the region in the center of the image is in the under the occulter
  ; region
  occulter_region = lr[nx / 2, ny / 2]

  ; find the centroid of the under the occulter region
  ind = where(lr eq occulter_region, count)
  xy = array_indices(lr, ind)
  center = reform(mean(xy, dimension=2))

  ; initial guess for the center of the image

  return, center 

end
