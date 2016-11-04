; docformat = 'rst'

;+
; Extract various image geometry parameters from the appropriate flat file.
;
; :Uses:
;   comp_inventory_header, comp_extract_time, comp_read_flats, sxpar
;
; :Returns:
;   structure
;
; :Params:
;   images : in, required, type="fltarr(620, 620, nimg)"
;     images to use to find field stop/occulter centers
;   headers : in, required, type="strarr(ntags, nimg)"
;     the fits headers from which we want the geometry
;   date_dir : in, required, type=string
;     the directory for containing the files for the date in question, used to
;     find the flat file
;
; :Author:
;   Joseph Plowman
;-
function comp_image_geometry, images, headers, date_dir
  @comp_constants_common

  ; scan the headers to find out what observations the files contain
  comp_inventory_header, headers, beam, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret

  ; get the time in the format preferred by read_flats
  time = comp_extract_time(headers, day, month, year, hours, mins, secs)
  comp_read_flats, date_dir, wave, beam, time, flat, flat_header, flat_waves, $
                   flat_names, flat_expose

  ; TODO: are the below correct? are we correcting for difference between
  ; FITS and IDL standards (off by 1)?

  ; TODO: (0, 0) is bottom left or top right?
  
  ; TODO: actually use COMP_FIND_ANNULUS to find, don't just
  ; read from flats
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
  pang1 = sxpar(flat_header, 'POSTANG1')
  pang2 = sxpar(flat_header, 'POSTANG2')

  ; overlap P angle (from the field stop)
  delta_x = sxpar(flat_header, 'OXCNTER2') - sxpar(flat_header, 'OXCNTER1')
  delta_y = sxpar(flat_header, 'OYCNTER1') - sxpar(flat_header, 'OYCNTER2')
  overlap_angle = !radeg * atan(delta_y / delta_x)

  return, { occulter1: occulter1, $
            occulter2: occulter2, $
            field1: field1, $
            field2: field2, $
            post_angle1: pang1, $
            post_angle2: pang2, $
            delta_x: delta_x, $
            delta_y: delta_y, $
            overlap_angle: overlap_angle $
          }
end
