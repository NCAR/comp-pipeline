; docformat = 'rst'

;+
; Create a mask for CoMP images in the 1024x1024 spatial resolution masking only
; the upper left beam.
;
; :Returns:
;   `bytarr`
;
; :Params:
;   flat_header : in, required, type=strarr
;     header from flat
;
; :Keywords:
;   margin : in, required, type=float
;     margin to exclude around edge of occulter and field
;
; :Author:
;   sitongia
;-
function comp_mask_1024_1, flat_header, margin=margin
  compile_opt strictarr
  @comp_constants_common
  @comp_mask_constants_common

  _margin = n_elements(margin) eq 0L ? 0.0 : margin

  field_overlap = 0

  ; get parameters from FITS header
  
  ; occulter position
  occulter1 = {x:sxpar(flat_header, 'OXCNTER1') - nx / 2, $
               y:sxpar(flat_header, 'OYCNTER1') - 1024 + ny / 2, $
               r:sxpar(flat_header, 'ORADIUS1')}
  occulter2 = {x:sxpar(flat_header, 'OXCNTER2') - 1024 + nx / 2, $
               y:sxpar(flat_header, 'OYCNTER2') - ny / 2, $
               r:sxpar(flat_header, 'ORADIUS2')}
  
  ; field position
  field1 = {x:sxpar(flat_header, 'FXCNTER1') - nx / 2, $
            y:sxpar(flat_header, 'FYCNTER1') - 1024 + ny / 2, $
            r:sxpar(flat_header, 'FRADIUS1')}
  field2 = {x:sxpar(flat_header, 'FXCNTER2') - 1024 + nx / 2, $
            y:sxpar(flat_header, 'FYCNTER2') - ny / 2, $
            r:sxpar(flat_header, 'FRADIUS2')}
  
  ; P angles of post
  post_angle1 = sxpar(flat_header, 'POSTANG1')
  post_angle2 = sxpar(flat_header, 'POSTANG2')

  ; occulter mask
  radius = (occulter1.r + occulter2.r) / 2.0 + _margin
  dmask1 = comp_disk_mask(radius, $
                          xcen=occulter1.x + (nx - 1.0) / 2.0, $
                          ycen=occulter1.y + (ny - 1.0) / 2.0)
  dmask2 = comp_disk_mask(radius, $
                          xcen=occulter2.x + (nx - 1.0) / 2.0, $
                          ycen=occulter2.y + (ny - 1.0) / 2.0)

  ; field mask
  fradius = (field1.r + field2.r) / 2.0 - _margin
  field_mask_1 = comp_field_mask(fradius, $
                                 xcen=field1.x + (nx - 1.0) / 2.0, $
                                 ycen=field1.y + (ny - 1.0) / 2.0)
  field_mask_2 = comp_field_mask(fradius, $
                                 xcen=field2.x + (nx - 1.0) / 2.0, $
                                 ycen=field2.y + (ny - 1.0) / 2.0)
  
  ; post mask
  pmask1 = comp_post_mask(post_angle1, 90.0)
  pmask2 = comp_post_mask(post_angle2, 90.0)
    
  mask1 = dmask1 * field_mask_1 * pmask1
  mask2 = dmask2 * field_mask_2 * pmask2
    
  ; construct large mask
  mask_image = bytarr(1024, 1024)
  mask_image[0:nx - 1, 1024 - nx:1024 - 1] += mask1
  
  ; mask out overlap
  ; new field masks, slightly larger, to create larger overlap to mask
  overlap_mask_1 = comp_field_mask(field1.r + field_overlap, $
                                   xcen=field1.x + (nx - 1.0) / 2.0, $
                                   ycen=field1.y + (ny - 1.0) / 2.0)
  overlap_mask_2 = comp_field_mask(field2.r + field_overlap, $
                                   xcen=field2.x + (nx - 1.0) / 2.0, $
                                   ycen=field2.y + (ny - 1.0) / 2.0)
    
  ; identify the overlap of images, from the field positions
  tmp_img = bytarr(1024, 1024)
  tmp_img[0:nx - 1, 1024 - nx:1024 - 1] += overlap_mask_1
  tmp_img[1024 - nx:1024 - 1, 0:nx - 1] += overlap_mask_2
  overlap = where(tmp_img gt 1B, count)
  if (count eq 0) then mg_log, 'no overlap', name='comp', /warn

  ; set first four columns to 1 so that they will not be used
  if (keyword_set(nullcolumns)) then begin  
    mask_image[0:3, *] = 1B
  endif

  if (count gt 0L) then mask_image[overlap] = 0B

  return, mask_image
end
