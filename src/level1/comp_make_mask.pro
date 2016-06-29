; docformat = 'rst'

;+
; Create a mask for CoMP images in the 620x620 spatial resolution. Include the
; occulting disk, field stop, occulter post and the overlap of the two
; sub-images.
;
; THIS ASSUMES THAT THE IMAGE HAS ALREADY BEEN CENTERED USING THE CENTER OF THE
; OCCULTER
;
; :Examples:
;   For example, call like::
;
;     comp_make_mask,'20130915',header,mask
;
; :Uses:
;   comp_constants_common, comp_mask_constants_common, comp_initialize,
;   comp_disk_mask, comp_field_mask, comp_post_mask, comp_overlap_mask
;
; :Returns:
;   mask image, `fltarr(1024, 1024)`
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   fits_header : in, required
;     the primary header of the CoMP FITS file
;   mask : out, optional, type="fltarr(1024, 1024)"
;     mask image
;
; :Author:
;   sitongia@hao.ucar.edu
;
; :History:
;    added comments 10/23/14 ST
;    removed post_rotation fudge factor 11/14/14 ST
;-
pro comp_make_mask, date_dir, fits_header, mask
  compile_opt strictarr
  @comp_constants_common
  @comp_mask_constants_common

  ; configure
  comp_initialize, date_dir

  ; get parameters from FITS header

  ; look for new keyword
  fradius = sxpar(fits_header, 'FRADIUS', count=count)

  if (count eq 0) then begin
    ; old keywords
    occulter = {x:sxpar(fits_header, 'CRPIX1'), $
                y:sxpar(fits_header, 'CRPIX2'), $
                r:((sxpar(fits_header, 'OCRAD1') $
                    + sxpar(fits_header, 'OCRAD2')) / 2.0)}
    field = {x:((sxpar(fits_header, 'FCENX1') $
                 + sxpar(fits_header, 'FCENX2')) / 2.0), $
             y:((sxpar(fits_header, 'FCENY1') $
                 + sxpar(fits_header, 'FCENY2')) / 2.0), $
             r:((sxpar(fits_header, 'FCRAD1') $
                 + sxpar(fits_header, 'FCRAD2')) / 2.0)}

    ; create the mask from individual masks

    ; occulter mask
    dmask = comp_disk_mask(occulter.r + occulter_offset)

    ; field mask
    field_mask = comp_field_mask(field.r + field_offset, $
                                 dx=(occulter.x - field.x), $
                                 dy=(occulter.y - field.y))

    mask = dmask * field_mask
  endif else begin
    occulter = {x:sxpar(fits_header, 'CRPIX1'), $
                y:sxpar(fits_header, 'CRPIX2'), $
                r:sxpar(fits_header, 'ORADIUS')}
    field = {x:sxpar(fits_header, 'FRPIX1'), $
             y:sxpar(fits_header,'FRPIX2'), $
             r:sxpar(fits_header,'FRADIUS')}
    post_angle = sxpar(fits_header, 'POSTPANG')
    overlap_angle = sxpar(fits_header, 'OVRLPANG')
    p_angle = sxpar(fits_header, 'SOLAR_P0')

    ; create the mask from individual masks

    ; occulter mask
    dmask = comp_disk_mask(occulter.r + occulter_offset)

    ; field mask
    field_mask = comp_field_mask(field.r + field_offset, $
                                 dx=field.x - occulter.x, $
                                 dy=field.y - occulter.y)

    ; post mask
    ; pmask = comp_post_mask(post_angle + 180. - p_angle - post_rotation, 32.0)      ST 11/14/14
    pmask = comp_post_mask(post_angle + 180. - p_angle, 32.0)

    ; overlap mask
    omask = comp_overlap_mask(field.r, overlap_angle + p_angle, $
                              dx=(occulter.x - field.x), $
                              dy=(occulter.y - field.y))

    mask = dmask * field_mask * pmask * omask
  endelse
end
