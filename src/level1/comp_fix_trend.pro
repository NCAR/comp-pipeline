; docformat = 'rst'

;+
; Fit trend across annulus. Removes fit and inserts annulus back into 1024
; image.
;
; :Todo:
;   field masking is in undestorted coordinates, so isn't a good fit for this
;
; :Params:
;    image_full
;    occulter1
;    occulter2
;    field1
;    field2
;    fit
;
; :Author:
;   sitongia
;-
pro comp_fix_trend, image_full, occulter1, occulter2, field1, field2, $
                    post_angle1, post_angle2, fit
  compile_opt strictarr
  @comp_constants_common
  @comp_mask_constants_common

  mask_for_fit = comp_mask_1024(occulter1, occulter2, $
                                field1, field2, $
                                post_angle1, post_angle2, $
                                o_offset=+5.0, f_offset=-4.0)

  mask_for_app = comp_mask_1024(occulter1, occulter2, $
                                field1, field2, $
                                post_angle1, post_angle2, $
                                o_offset=0.0, f_offset=0.0)

  ; sub-image 1
  flat = comp_extract1(image_full) 
  mask = comp_extract1(mask_for_fit)

  ; Use the field mask to eliminate the crescent of the other beam that is the sub-image
  field_mask = comp_field_mask(field1.r, dx=field1.x, dy=field1.y)
  mask *= field_mask
  fit1 = comp_detrender(flat, mask)

  detrended_flat = flat
  mask = comp_extract1(mask_for_app)
  good = where(mask)
  ; detrended_flat[good]=flat[good] / fit1[good]
  detrended_flat = flat / fit1

  ; put back into 1024 image
  tmp_mask = fltarr(1024, 1024)
  dmask = comp_disk_mask(occulter1.r, dx=occulter1.x, dy=occulter1.y)
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
  field_mask = comp_field_mask(field2.r, dx=field2.x, dy=field2.y)
  mask *= field_mask
  fit2 = comp_detrender(flat, mask)

  detrended_flat = flat
  mask = comp_extract2(mask_for_app)
  good = where(mask)
  ; detrended_flat[good]=flat[good] / fit2[good]
  detrended_flat = flat / fit2

  ; put back into 1024 image
  tmp_mask = fltarr(1024,1024)
  dmask = comp_disk_mask(occulter2.r, dx=occulter2.x, dy=occulter2.y)
  tmp_mask[1024L - nx:1024L - 1L, 0L:nx - 1L] = dmask * field_mask
  good = where(tmp_mask)
  tmp_flat = fltarr(1024, 1024)
  tmp_flat[1024L - nx:1024L - 1L, 0L:nx - 1L] = detrended_flat
  image_full[good] = tmp_flat[good]

  fit = (fit1 + fit2) / 2.0
end
