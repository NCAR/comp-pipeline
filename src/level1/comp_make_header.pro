; docformat = 'rst'

;+
; Procedure to determine the location of the occulting disk and field stop in
; each beam of a comp large image and put them into a fits header.
;
; :Uses:
;   comp_constants_common, comp_find_annulus, comp_find_post, comp_extract1,
;   comp_extract2, mkhdr, sxaddpar
;
; :Params:
;   image : in
;     the large format comp image
;   header : out
;     the resulting fits header
;   occulter1 : out
;     structure containing the parameters of the occulting disk `{x,y,radius}`
;     for subimage 1 (pixels)
;   field1 : out
;     structure containing the parameters of the field stop `{x,y,radius}` for
;     subimage 1 (pixels) 
;   post_angle1 : out
;     the position angle of the post for subimage 1 (degrees)
;   occulter2 : out
;     structure containing the parameters of the occulting disk `{x,y,radius}`
;     for subimage 2 (pixels)
;   field2 : out
;     structure containing the parameters of the field stop `{x,y,radius}` for
;     subimage 2 (pixels)
;   post_angle2 : out
;     the position angle of the post for subimage 2 (degrees)
;
; :Author:
;   sitongia, modified by Tomczyk
;
; :History:
;   added comments - 10/23/14 ST
;-
pro comp_make_header, image, header, $
                      occulter1, field1, post_angle1, $
                      occulter2, field2, post_angle2
  compile_opt strictarr
  @comp_constants_common

  mkhdr, header, image, /image

  ; restrieve distortion coefficients in file: dx1_c, dy1_c, dx2_x, dy2_c
  restore, filename=distortion_coeffs_file

  flat1 = comp_extract1(image)   ; extract the subimage
  flat2 = comp_extract2(image)   ; extract the subimage

  ; remove distortion (NOTE: these images will not be saved!)
  comp_apply_distortion, flat1, flat2, dx1_c, dy1_c, dx2_c, dy2_c

  ; image 1
  comp_find_annulus, flat1, occulter1, field1, error=error
  if (error ne 0L) then message, 'error finding image center'
  comp_find_post, flat1, occulter1, field1, post_angle1

  ; image 2
  comp_find_annulus, flat2, occulter2, field2, error=error
  if (error ne 0L) then message, 'error finding image center'
  comp_find_post, flat2, occulter2, field2, post_angle2

  ; occulter position
  sxaddpar, header, 'OXCNTER1', occulter1.x + nx / 2, $
            ' Occulter center X for sub-image 1'
  sxaddpar, header, 'OYCNTER1', occulter1.y + 1024 - nx / 2, $
            ' Occulter center Y for sub-image 1'
  sxaddpar, header, 'ORADIUS1', occulter1.r, $
            ' Occulter Radius for sub-image 1'
  sxaddpar, header, 'OXCNTER2', occulter2.x + 1024 - nx / 2, $
            ' Occulter center X for sub-image 2'
  sxaddpar, header, 'OYCNTER2', occulter2.y + nx / 2, $
            ' Occulter center Y for sub-image 2'
  sxaddpar, header, 'ORADIUS2', occulter2.r, $
            ' Occulter Radius for sub-image 2'

  ; field position
  sxaddpar, header, 'FXCNTER1', field1.x + nx / 2, $
            ' Field Stop center X for sub-image 1'
  sxaddpar, header, 'FYCNTER1', field1.y + 1024 - nx / 2, $
            ' Field Stop center Y for sub-image 1'
  sxaddpar, header, 'FRADIUS1', field1.r, $
            ' Field Stop Radius for sub-image 1'
  sxaddpar, header, 'FXCNTER2', field2.x + 1024 - nx / 2, $
            ' Field Stop center X for sub-image 2'
  sxaddpar, header, 'FYCNTER2', field2.y + nx / 2, $
            ' Field Stop center Y for sub-image 2'
  sxaddpar, header, 'FRADIUS2', field2.r, $
            ' Field Stop Radius for sub-image 2'

  ; position angles for post
  sxaddpar, header, 'POSTANG1', post_angle1, $
            ' Position Angle of post for sub-image 1'
  sxaddpar, header, 'POSTANG2', post_angle2, $
            ' Position Angle of post for sub-image 2'
end
