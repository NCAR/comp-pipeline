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

  ; TODO: this threshold should be a function of wave_type (this might be an
  ; issue for 1083, but not 1074 or 1079)
  threshold = 250.0

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
