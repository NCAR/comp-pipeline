; docformat = 'rst'

;+
; Create a mask for CoMP images in the 1024x1024 spatial resolution. Include
; the occulting disk, field stop, occulter post and the overlap of the two
; sub-images in the 1024x1024 format.
;
; :Uses:
;   comp_constants_common, comp_mask_constants_common, comp_disk_mask,
;   comp_field_mask, comp_post_mask
;
; :Returns:
;   mask image, `fltarr(1024, 1024)`
;
; :Params:
;   occulter1 : in, required, type=structure
;     structure for occulter in sub-image 1 of the form `{x:0.0, y:0.0, r:0.0}`
;   occulter2 : in, required, type=structure
;     structure for occulter in sub-image 2 of the form `{x:0.0, y:0.0, r:0.0}`
;   field1 : in, required, type=structure
;     structure for field in sub-image 1 of the form `{x:0.0, y:0.0, r:0.0}`
;   field2 : in, required, type=structure
;     structure for field in sub-image 2 of the form `{x:0.0, y:0.0, r:0.0}`
;   post_angle1 : in, required, type=float
;     position angle for post in sub-image 1
;   post_angle2 : in, required, type=float
;     position angle for post in sub-image 2
;
; :Keywords:
;   o_offset : in, optional, type=float, default=occulter_offset
;     number of pixels to add/subtract to radius of occulter for mask
;   f_offset : in, optional, type=float, default=field_offset
;     number of pixels to add/subtract to field of occulter for mask
;   bc1 : in, optional, type=float, default=1.0
;     background solar spectrum correction for sum-image1
;   bc2 : in, optional, type=float, default=1.0
;     background solar spectrum correction for sum-image2
;   nopost : in, optional, type=boolean
;     don't include post mask in final mask
;   nooverlap : in, optional, type=boolean
;     don't include overlap in final mask
;   nullcolumns : in, optional, type=boolean
;     if set, null out first four columns
;
; :Author:
;   Sitongia
;-
function comp_mask_1024, occulter1, occulter2, $
                         field1, field2, $
                         post_angle1, post_angle2, $
                         o_offset=o_offset, f_offset=f_offset, $
                         bc1=bc1, bc2=bc2, $
                         nopost=nopost, $
                         nooverlap=nooverlap, $
                         nullcolumns=nullcolumns
  compile_opt strictarr
  @comp_constants_common
  @comp_mask_constants_common

  if (n_elements(o_offset) eq 1) then begin
    local_o_offset = o_offset
  endif else begin
    local_o_offset = occulter_offset
  endelse

  if (n_elements(f_offset) eq 1) then begin
    local_f_offset = f_offset
  endif else begin
    local_f_offset = field_offset
  endelse
  
  if (n_elements(bc1) eq 1) then begin
    local_bc1 = bc1
  endif else begin
    local_bc1 = 1.0
  endelse
  
  if (n_elements(bc2) eq 1) then begin
    local_bc2 = bc2
  endif else begin
    local_bc2 = 1.0
  endelse

  ; occulter mask
  radius = (occulter1.r + occulter2.r) * 0.5
  dmask1 = comp_disk_mask(radius + local_o_offset, dx=occulter1.x, dy=occulter1.y)
  dmask2 = comp_disk_mask(radius + local_o_offset, dx=occulter2.x, dy=occulter2.y)

  ; field mask
  fradius = (field1.r + field2.r) * 0.5
  field_mask_1 = comp_field_mask(fradius + local_f_offset, dx=field1.x, dy=field1.y)
  field_mask_2 = comp_field_mask(fradius + local_f_offset, dx=field2.x, dy=field2.y)

  ; post mask
  if n_elements(nopost) eq 0 then begin
    pmask1 = comp_post_mask(post_angle1, 90.0)
    pmask2 = comp_post_mask(post_angle2, 90.0)

    mask1 = dmask1 * field_mask_1 * pmask1
    mask2 = dmask2 * field_mask_2 * pmask2
  endif else begin
    mask1 = dmask1 * field_mask_1
    mask2 = dmask2 * field_mask_2
  endelse

  ; construct large mask
  mask_image = fltarr(1024, 1024)
  mask_image[0:nx - 1, 1024 - nx:1024 - 1] $
    = mask_image[0:nx - 1, 1024 - nx:1024 - 1] + mask1 / local_bc1
  mask_image[1024 - nx:1024 - 1,0:nx - 1] $
    = mask_image[1024 - nx:1024 - 1, 0:nx - 1] + mask2 / local_bc2

  ; mask out overlap
  if (n_elements(nooverlap) eq 0) then begin
    ; wew field masks, slightly larger, to create larger overlap to mask
    overlap_mask_1 = comp_field_mask(field1.r + field_overlap, dx=field1.x, dy=field1.y)
    overlap_mask_2 = comp_field_mask(field2.r + field_overlap, dx=field2.x, dy=field2.y)

    ; identify the overlap of images, from the field positions
    tmp_img = fltarr(1024,1024)
    tmp_img[0:nx - 1, 1024 - nx:1024 - 1] = tmp_img[0:nx - 1, 1024 - nx:1024 - 1] $
                                              + overlap_mask_1
    tmp_img[1024 - nx:1024 - 1, 0:nx - 1] = tmp_img[1024 - nx:1024 - 1, 0:nx - 1] $
                                              + overlap_mask_2
    overlap = where(tmp_img gt 1.0, count)
    if (count eq 0) then begin
      mg_log, 'no overlap', name='comp', /warn
    endif

    mask_image[overlap] = 0.0
  endif

  ; set first four columns to 1 so that they will not be used
  if (keyword_set(nullcolumns)) then begin  
    mask_image[0:3, *] = 1.0
  endif

  return, mask_image
end
