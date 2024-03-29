; docformat = 'rst'

;+
; Fit trend across annulus. Removes fit and inserts annulus back into 1024
; image.
;
; :Todo:
;   field masking is in undistorted coordinates, so isn't a good fit for this
;
; :Params:
;   image_full : in, required, type="fltarr(1024, 1024)"
;     raw image
;   occulter1 : in, required, type=structure
;     structure for occulter in sub-image 1
;   occulter2 : in, required, type=structure
;     structure for occulter in sub-image 1
;   field1 : in, required, type=structure
;     structure for field in sub-image 1
;   field2 : in, required, type=structure
;     structure for field in sub-image 1
;   post_angle1 : in, required, type=float
;     position angle for post in sub-image 1
;   post_angle2 : in, required, type=float
;     position angle for post in sub-image 2
;   fit : out, optional, type=fltarr
;     fit
;
; :Author:
;   MLSO Software Team
;-
pro comp_fix_trend, image_full, occulter1, occulter2, field1, field2, $
                    post_angle1, post_angle2, fit
  compile_opt strictarr
  @comp_constants_common
  @comp_mask_constants_common

  mask_for_fit = comp_mask_1024(occulter1, occulter2, $
                                field1, field2, $
                                post_angle1, post_angle2, $
                                o_offset=+5.0, f_offset=-4.0, /nopost)

  mask_for_app = comp_mask_1024(occulter1, occulter2, $
                                field1, field2, $
                                post_angle1, post_angle2, $
                                o_offset=0.0, f_offset=0.0, /nopost)

  ; sub-image 1
  flat = comp_extract1(image_full) 
  mask = comp_extract1(mask_for_fit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TODO check that center changes are included properly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Use the field mask to eliminate the crescent of the other beam that is the sub-image
  field_mask = comp_field_mask(field1.r, xcen=field1.x, ycen=field1.y)
  mask *= field_mask
  fit1 = comp_detrender(flat, mask)

  detrended_flat = flat
  mask = comp_extract1(mask_for_app)
  good = where(mask)
  ; detrended_flat[good]=flat[good] / fit1[good]
  detrended_flat = flat / fit1

  ; put back into 1024 image
  tmp_mask = fltarr(1024, 1024)
  dmask = comp_disk_mask(occulter1.r, xcen=occulter1.x, ycen=occulter1.y)
  tmp_mask[0L:nx - 1L, 1024L - nx:1024L - 1L] = dmask * field_mask
  good = where(tmp_mask)
  tmp_flat = fltarr(1024,1024)
  tmp_flat[0L:nx - 1L, 1024L - nx:1024L - 1L] = detrended_flat
  image_full[good] = tmp_flat[good]

  ; sub-image 2
  flat = comp_extract2(image_full)
  mask = comp_extract2(mask_for_fit)

  ; use the field mask to eliminate the crescent of the other beam that is the
  ; sub-image
  field_mask = comp_field_mask(field2.r, xcen=field2.x, ycen=field2.y)
  mask *= field_mask
  fit2 = comp_detrender(flat, mask)

  detrended_flat = flat
  mask = comp_extract2(mask_for_app)
  good = where(mask)
  ; detrended_flat[good]=flat[good] / fit2[good]
  detrended_flat = flat / fit2

  ; put back into 1024 image
  tmp_mask = fltarr(1024,1024)
  dmask = comp_disk_mask(occulter2.r, xcen=occulter2.x, ycen=occulter2.y)
  tmp_mask[1024L - nx:1024L - 1L, 0L:nx - 1L] = dmask * field_mask
  good = where(tmp_mask)
  tmp_flat = fltarr(1024, 1024)
  tmp_flat[1024L - nx:1024L - 1L, 0L:nx - 1L] = detrended_flat
  image_full[good] = tmp_flat[good]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  fit = (fit1 + fit2) / 2.0
end
