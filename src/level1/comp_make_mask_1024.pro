; docformat = 'rst'

;+
; Create a mask for CoMP images in the 1024x1024 spatial resolution. Include
; the occulting disk, field stop, occulter post and the overlap of the two
; sub-images in the 1024x1024 format.
;
; Note: the positions in the image are as found in the opal data and have not
; been translated or rotated.
;
; :Examples:
;   For example, call like::
;
;     cd, '/hao/kaula1/Data/CoMP/process/20120409'
;     fits_read, 'flat.fts', d, flat_header, /header_only, exten_no=1
;     comp_make_mask_1024, '20120409', flat_header, o_offset=-3.0, f_offset=+3.0, mask
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   flat_header : in
;     the primary header of the CoMP FITS flat file
;   mask : out
;     the mask image
;
; :Keywords:
;   o_offset
;     number of pixels to add/subtract to radius of occulter for mask
;   f_offset
;     number of pixels to add/subtract to field of occulter for mask
;   nopost : in, optional, type=boolean
;
; :Author:
;   sitongia@hao.ucar.edu
;-
pro comp_make_mask_1024, date_dir, flat_header, mask, $
                         o_offset=o_offset, f_offset=f_offset, $
                         nopost=nopost
  compile_opt idl2
  @comp_constants_common
  @comp_mask_constants_common

  ; configure
  comp_initialize, date_dir

  ; get parameters from FITS header
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
  
  mask = comp_mask_1024(occulter1, occulter2, $
                        field1, field2, $
                        post_angle1, post_angle2, $
                        o_offset=o_offset, f_offset=f_offset, $
                        nopost=nopost
end