; docformat = 'rst'

;+
; comp_promote_primary_header_l1
;
; Updates a CoMP raw primary header to a level 1 header.
; 
; :Params:
;   headers : in, required, type='strarr(ntags, nimg)'
;     the array of headers (with extensions included) read from the CoMP raw
;     data file
;   primary_header : in, required, type=strarr(nptags)
;     the primary header from the raw data file; updated to L1 on output
;   date_dir : in, required, type = string
;     the directory for containing the files for the date in question, used to
;     find the flat file.
;
; :Author:
;   Joseph Plowman
;-
pro comp_promote_primary_header_l1, headers, primary_header, date_dir
  compile_opt strictarr

  @comp_mask_constants_common

  comp_inventory_header, headers, beam, group, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret
  time = comp_extract_time(headers, day, month, year, hours, mins, secs)
  num_wave = n_elements(wave(uniq(wave, sort(wave))))

  ; Get the comp image geometry (field stop, occulter, etc):
  comp_image_geometry, headers, date_dir, $
                       occulter1, occulter2, $
                       field1, field2, $
                       post_angle1, post_angle2, $
                       delta_x, delta_y, $
                       overlap_angle

  ; TODO: we should use SUN for this

  ; compute solar ephemeris quantities from date and time
  jd = tojd(day, month, year, hours, mins, secs) + 10.0 / 24.0
  ephem2, jd, sol_ra, sol_dec, b0, p_angle, semi_diam, sid_time, dist, $
          xsun, ysun, zsun

  nx = 620
  ny = nx

  ; get rid of all the blank comments
  sxdelpar, primary_header, 'COMMENT'

  ; change the processing level
  sxaddpar, primary_header, 'LEVEL','L1', ' Processing Level'
  sxaddpar, primary_header, 'DATE-CAL', datecal(), ' Date of calibration processing'
  sxaddpar, primary_header, 'VERSION', 'JEP 0.01', ' demod Software Version'
  sxaddpar, primary_header, 'NTUNE', num_wave, ' Number of wavelength tunings', before='TNELNGTH'
  sxaddpar, primary_header, 'OBJECT', 'corona', ' Coronal Emission', after='LOCATION'
  sxaddpar, primary_header, 'BUNIT', 'MILLIONTHS', ' Millions of brightness of solar disk'
  sxaddpar, primary_header, 'METHOD', 'mean', ' Averaging method'
  sxaddpar, primary_header, 'DESTRAY', 'NO', ' Destraying Applied'
  sxaddpar, primary_header, 'COORDNAM', ' HELIOCENTRIC', 'COORDINATE SYSTEM NAME'
  sxaddpar, primary_header, 'CTYPE1', 'X', ' AXIS 1 TYPE: X [EAST->WEST ] GEOCENTRIC'
  sxaddpar, primary_header, 'CTYPE2', 'Y', ' AXIS 2 TYPE: Y [SOUTH->NORTH] GEOCENTRIC'

  ; occulter (Sun center) parameters
  sxaddpar, primary_header, 'CRPIX1', nx / 2 + 0.5, ' X [EAST->WEST ] SUN CENTER [PIXELS]'
  sxaddpar, primary_header, 'CRVAL1', 0.0, ' X [EAST->WEST ] SUN CENTER [ARCSEC]'
  sxaddpar, primary_header, 'CDELT1', plate_scale, ' solar_X coord increment [arcsec/pixel]'
  sxaddpar, primary_header, 'CROTA1', 0.0, ' X [EAST->WEST ] ROTATION [DEG.] FROM REFERENCE'
  sxaddpar, primary_header, 'ORADIUS', (occulter1.r+occulter2.r)/2., ' [pixels] Occulter Radius', format='(f8.2)'
  sxaddpar, primary_header, 'CRPIX2', ny / 2 + 0.5, ' Y [SOUTH->NORTH] SUN CENTER [PIXELS]'
  sxaddpar, primary_header, 'CRVAL2', 0.0, ' Y [SOUTH->NORTH] SUN CENTER [ARCSEC]'
  sxaddpar, primary_header, 'CDELT2', plate_scale, ' solar_Y coord increment [arcsec/pixel]'
  sxaddpar, primary_header, 'CROTA2', 0.0, ' Y [SOUTH->NORTH] ROTATION [DEG.] FROM REFERENCE'

  ; field parameters
  sxaddpar, primary_header, 'FRADIUS', (field1.r + field1.r) / 2., ' [pixels] Field Radius', format='(f8.2)'
  ; Center of field, offset from center of occulter and rotated by angle
  xoffset = ((field1.x + field2.x) / 2.0 - (occulter1.x + occulter2.x) / 2.0)
  yoffset = ((field1.y + field2.y) / 2.0 - (occulter1.y + occulter2.y) / 2.0)

  ; TODO is the following right?
  fxcent = nx / 2.0 + 0.5 $
             + xoffset * cos(- p_angle * !pi / 180.0) $
             - yoffset * sin(- p_angle * !pi / 180.0)
  fycent = ny / 2.0 + 0.5 $
             + xoffset * sin(- p_angle * !pi / 180.0) $
             + yoffset * cos(- p_angle * !pi / 180.0)
  sxaddpar, primary_header, 'FRPIX1', fxcent, ' [pixels] X [EAST->WEST ] FIELD CENTER [PIXELS]', format='(f8.2)'
  sxaddpar, primary_header, 'FRPIX2', fycent, ' [pixels] Y [SOUTH->NORTH] FIELD CENTER [PIXELS]', format='(f8.2)'

  ; post P angle
  sxaddpar, primary_header, 'POSTPANG', (post_angle1 + post_angle2) / 2.0, ' [degrees] P Angle of occulter post'

  ; overlap P angle (from the field stop)
  sxaddpar, primary_header, 'OVRLPANG', overlap_angle, ' [degrees] P Angle of field overlap'

  ; occulter ID and size
  occ_id=sxpar(primary_header,'OCCULTER')
  sxdelpar,primary_header,'OCCULTER'
  sxaddpar, primary_header, 'OCC-ID', occ_id, ' Occulter Identification Number'
  occulter_size = occulter_id(occ_id)
  sxaddpar, primary_header, 'OCC-SIZE', occulter_size, ' [mm] Occulter size'

  ; ephemeris information
  sxaddpar, primary_header, 'RSUN', semi_diam, ' [arcsec] Solar Radius', format='(f8.2)'
  sxaddpar, primary_header, 'SOLAR_P0', p_angle, ' [degrees] P Angle', format='(f8.2)'
  sxaddpar, primary_header, 'SOLAR_B0', b0, ' [degrees] B Angle', format='(f8.2)'
  ; TODO  sxaddpar, primary_header,'SOLAR_L0',l0, ' [degrees] L Angle'
  sxaddpar, primary_header, 'SOLAR_RA', sol_ra, ' [HOURS] Solar Right Ascension', format='(f8.3)'
  sxaddpar, primary_header, 'SOLARDEC', sol_dec, ' [degrees] Solar Declination', format='(f8.2)'

  ; fix the date/time in UT
  fix_header_time, primary_header

  ; static I to Q and U crosstalk coefficients
  i_to_q = -0.000581
  i_to_u =  0.004841
  sxaddpar, primary_header, 'i_to_q', i_to_q, ' Crosstalk coefficient from I to Q'
  sxaddpar, primary_header, 'i_to_u', i_to_u, ' Crosstalk coefficient from I to U'
end
