; docformat = 'rst'

;+
; Extract various image geometry parameters from the appropriate flat file.
;
; :Uses:
;   comp_inventory_header, comp_extract_time, comp_read_flats, sxpar
;
; :Params:
;   headers : in, required, type="strarr(ntags, nimg)"
;     the fits headers from which we want the geometry
;   date_dir : in, required, type=string
;     the directory for containing the files for the date in question, used to
;     find the flat file
;   occulter1 : out, required, type=structure
;     structure with coordinates of the first occulter
;   occulter2 : out, required, type=structure
;     structure with coordinates of the second occulter
;   field1 : out, required, type=structure
;     structure with coordinates of first field stop
;   field2 : out, required, type=structure
;     structure with coordinates of second field stop
;   pang1 : out, required, type=float
;     angle of first post
;   pang2 : out, required, type=float
;     angle of second post
;   overlap_angle : out, required, type=float
;     overlap P angle (from the field stop)
;
; :Author:
;   Joseph Plowman
;-
pro comp_image_geometry, headers, date_dir, $
                         occulter1, occulter2, $
                         field1, field2, $
                         pang1, pang2, $
                         dx, dy, $
                         overlap_angle

  ; scan the headers to find out what observations the files contain
  comp_inventory_header, headers, beam, group, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret

  ; get the time in the format preferred by read_flats
  time = comp_extract_time(headers, day, month, year, hours, mins, secs)
  comp_read_flats, date_dir, wave, beam, time, flat, flat_header, flat_waves, $
                   flat_names, flat_expose

  nx = 620
  ny = nx

  ; do once when using centers from flats
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
  delta_x = sxpar(flat_header, 'FXCNTER2') - sxpar(flat_header, 'FXCNTER1')
  delta_y = sxpar(flat_header, 'FYCNTER1') - sxpar(flat_header, 'FYCNTER2')
  overlap_angle = !radeg * atan(delta_y / delta_x)
end