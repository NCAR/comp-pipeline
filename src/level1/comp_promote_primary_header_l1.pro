; docformat = 'rst'

;+
; Updates a CoMP raw primary header to a level 1 header.
;
; :Uses:
;   comp_inventory_header, comp_extract_time, comp_fix_header_time,
;   comp_occulter_id, comp_mask_constants_common, tojd, sun, sxdelpar,
;   sxaddpar, sxpar
;
; :Params:
;   headers : in, required, type='strarr(ntags, nimg)'
;     the array of headers (with extensions included) read from the CoMP raw
;     data file
;   primary_header : in, required, type=strarr(ntags)
;     the primary header from the raw data file; updated to L1 on output
;   date_dir : in, required, type = string
;     the directory for containing the files for the date in question, used to
;     find the flat file.
;   wave_type : in, required, type=string
;     wavelength to process, '1074', '1079', etc.
;
; :Keywords:
;   image_geometry : in, required, type=structure
;     image geometry specifications
;   headers_combine : in, required, type=strarr
;     temporary headers L1 extension headers
;
; :Author:
;   Joseph Plowman
;-
pro comp_promote_primary_header_l1, headers, primary_header, date_dir, wave_type, $
                                    background=background, $
                                    image_geometry=image_geometry, $
                                    headers_combine=headers_combine
  compile_opt strictarr

  @comp_constants_common
  @comp_config_common
  @comp_mask_constants_common

  comp_inventory_header, headers_combine, beam, group, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret

  unique_wave = wave[uniq(wave, sort(wave))]

  unique_pol = pol[uniq(pol, sort(pol))]
  unique_pol = unique_pol[where(strmid(unique_pol, 0, 3) ne 'BKG')]
  pol_tag = strupcase(strjoin(unique_pol))

  time = comp_extract_time(headers, day, month, year, hours, mins, secs)
  num_wave = n_elements(wave[uniq(wave, sort(wave))])

  sun, year, month, day, 10.0 + hours + mins / 60. + secs / 3600., $
       pa=p_angle, sd=semi_diam, true_ra=sol_ra, true_dec=sol_dec, lat0=b0
  sol_ra *= 15  ; convert from hours to degrees, 15 = 360 / 24

  ; get rid of all the blank comments
  sxdelpar, primary_header, 'COMMENT'

  sxaddpar, primary_header, 'BITPIX', -32
  sxdelpar, primary_header, 'OBSERVER'

  ; change the processing level
  sxaddpar, primary_header, 'LEVEL','L1', ' Processing Level'
  sxaddpar, primary_header, 'DATE-CAL', datecal(), $
            ' Date of calibration processing'
  sxaddpar, primary_header, 'VERSION', code_version, $
            ' Calibration processing software version'
  sxaddpar, primary_header, 'REVISION', code_revision, $
            ' Calibration processing software revision'

  sxaddpar, primary_header, 'NTUNES', num_wave, $
            ' Number of wavelength tunings', before='TNELNGTH'
  sxaddpar, primary_header, 'WAVETYPE', wave_type, $
            ' Wavelength type', after='NTUNES'
  sxaddpar, primary_header, 'WAVE_REF', unique_wave[n_elements(unique_wave) / 2], $
            ' [nm] Center wavelength', after='WAVETYPE', format='(F0.2)'
  sxaddpar, primary_header, 'WAVESTEP', unique_wave[1] - unique_wave[0], $
            ' [nm] Spacing between wavelengths', after='WAVE_REF', format='(F0.2)'
  sxaddpar, primary_header, 'WAVEFWHM', wavefwhm, $
            ' [nm] full width half max of bandpass filter', after='WAVESTEP', $
            format='(F0.2)'
  sxaddpar, primary_header, 'POL_LIST', pol_tag, $
            ' Unique polarization states', after='WAVEFWHM'
  sxaddpar, primary_header, 'TNELNGTH', sxpar(primary_header, 'TNELNGTH'), $
            ' Duration of Transient Pneumatic Effect Puls (ms)'

  sxaddpar, primary_header, 'POLANGLE', sxpar(primary_header, 'POLANGLE'), $
            format='(F0.3)'

  obs_id = sxpar(primary_header, 'OBS_ID', count=n_obs_id)
  comment = ' Name of Current Observing Seq.'
  if (n_obs_id gt 0) then begin
    obs_id = strtrim(obs_id, 2)
    sxaddpar, primary_header, 'OBS_ID', obs_id, $
              strmid(comment, 0, 80 - strlen(obs_id) - 2 - 8 - 1 - 1)
  endif else begin
    sxaddpar, primary_header, 'OBS_ID', '', comment, after='RETARDER'
  endelse

  obs_plan = sxpar(primary_header, 'OBS_PLAN', count=n_obs_plan)
  comment = ' Name of Current Observing Observing program'
  if (n_obs_plan gt 0) then begin
    obs_plan = strtrim(obs_plan, 2)
    sxaddpar, primary_header, 'OBS_PLAN', obs_plan, $
              strmid(comment, 0, 80 - strlen(obs_plan) - 2 - 8 - 1 - 1)
  endif else begin
    sxaddpar, primary_header, 'OBS_PLAN', '', comment, after='OBS_ID'
  endelse

  sxaddpar, primary_header, 'OBJECT', 'corona', ' Coronal Emission', after='LOCATION'
  sxaddpar, primary_header, 'BUNIT', '1.E-06 B/Bsun', $
            ' Intensity of Millionths of solar disk brightness'
  sxaddpar, primary_header, 'BZERO', 0, ' offset for unsigned integer data', after='BUNIT'
  sxaddpar, primary_header, 'BSCALE', 1.00, ' physical = data * BSCALE + BZERO', $
            after='BZERO', format='(F0.2)'

  sxaddpar, primary_header, 'METHOD', 'mean', ' Averaging method'
  sxaddpar, primary_header, 'COORDNAM', ' HELIOCENTRIC', 'COORDINATE SYSTEM NAME'
  sxaddpar, primary_header, 'CTYPE1', 'X', ' AXIS 1 TYPE: X [EAST->WEST ] HELIOCENTRIC'
  sxaddpar, primary_header, 'CTYPE2', 'Y', ' AXIS 2 TYPE: Y [SOUTH->NORTH] HELIOCENTRIC'

  ; occulter (Sun center) parameters
  sxaddpar, primary_header, 'CRPIX1', nx / 2 + 0.5, $
            ' X [EAST->WEST ] SUN CENTER [PIXELS]', format='(F0.2)'
  sxaddpar, primary_header, 'CRVAL1', 0.0, $
            ' X [EAST->WEST ] SUN CENTER [ARCSEC]', format='(F0.3)'
  sxaddpar, primary_header, 'CDELT1', plate_scale, $
            ' solar_X coord increment [arcsec/pixel]', format='(F0.2)'
  sxaddpar, primary_header, 'CROTA1', 0.0, $
            ' X [EAST->WEST ] ROTATION [DEG.] WRT TO SOLAR NORTH', format='(F0.2)'
  sxaddpar, primary_header, 'ORADIUS', $
            (image_geometry.occulter1.r + image_geometry.occulter2.r) / 2., $
            ' [pixels] Occulter Radius', format='(f8.2)'

  sxaddpar, primary_header, 'CRPIX2', ny / 2 + 0.5, $
            ' Y [SOUTH->NORTH] SUN CENTER [PIXELS]', format='(F0.2)'
  sxaddpar, primary_header, 'CRVAL2', 0.0, $
            ' Y [SOUTH->NORTH] SUN CENTER [ARCSEC]', format='(F0.3)'
  sxaddpar, primary_header, 'CDELT2', plate_scale, $
            ' solar_Y coord increment [arcsec/pixel]', format='(F0.2)'
  sxaddpar, primary_header, 'CROTA2', 0.0, $
            ' Y [SOUTH->NORTH] ROTATION [DEG.] WRT TO SOLAR NORTH', format='(F0.2)'

  ; field parameters
  sxaddpar, primary_header, 'FRADIUS', $
            (image_geometry.field1.r + image_geometry.field1.r) / 2., $
            ' [pixels] Field Radius', format='(f8.2)'
  ; Center of field, offset from center of occulter and rotated by angle
  xoffset = (image_geometry.occulter1.x + image_geometry.occulter2.x) / 2.0 $
              - (image_geometry.field1.x + image_geometry.field2.x) / 2.0
  yoffset = (image_geometry.occulter1.y + image_geometry.occulter2.y) / 2.0 $
              - (image_geometry.field1.y + image_geometry.field2.y) / 2.0

  fxcent = nx / 2.0 + 0.5 $
             + xoffset * cos(- p_angle * !pi / 180.0) $
             - yoffset * sin(- p_angle * !pi / 180.0)
  fycent = ny / 2.0 + 0.5 $
             + xoffset * sin(- p_angle * !pi / 180.0) $
             + yoffset * cos(- p_angle * !pi / 180.0)
  sxaddpar, primary_header, 'FRPIX1', fxcent, ' [pixels] X [EAST->WEST ] FIELD CENTER [PIXELS]', format='(f8.2)'
  sxaddpar, primary_header, 'FRPIX2', fycent, ' [pixels] Y [SOUTH->NORTH] FIELD CENTER [PIXELS]', format='(f8.2)'

  ; post P angle
  sxaddpar, primary_header, 'POSTPANG', $
            (image_geometry.post_angle1 + image_geometry.post_angle2) / 2.0, $
            ' [degrees] P Angle of occulter post'

  ; overlap P angle (from the field stop)
  sxaddpar, primary_header, 'OVRLPANG', image_geometry.overlap_angle, $
            ' [deg] Ang. of img. separation between camera diag. and optic plane', format='(F0.2)'

  ; occulter ID and size
  occ_id = sxpar(primary_header, 'OCCULTER')
  sxdelpar, primary_header, 'OCCULTER'
  sxaddpar, primary_header, 'OCC-ID', occ_id, ' Occulter Identification Number'

  occulter_size = comp_occulter_id(occ_id)
  sxaddpar, primary_header, 'OCC-SIZE', occulter_size, $
            ' [mm] Occulter size', format='(F0.3)'

  ; ephemeris information
  sxaddpar, primary_header, 'RSUN', semi_diam, ' [arcsec] Solar Radius', format='(f8.2)'
  sxaddpar, primary_header, 'SOLAR_P0', p_angle, ' [degrees] P Angle', format='(f8.2)'
  sxaddpar, primary_header, 'SOLAR_B0', b0, ' [degrees] B Angle', format='(f8.2)'
  ; TODO  sxaddpar, primary_header, 'SOLAR_L0', l0, ' [degrees] L Angle'
  sxaddpar, primary_header, 'SOLAR_RA', sol_ra, ' [HOURS] Solar Right Ascension', format='(f8.3)'
  sxaddpar, primary_header, 'SOLARDEC', sol_dec, ' [degrees] Solar Declination', format='(f8.2)'

  ; fix the date/time in UT
  comp_fix_header_time, primary_header

  ; I to Q and U crosstalk coefficients
  sxaddpar, primary_header, 'i_to_q', i_to_q_xtalk, ' Crosstalk coefficient from I to Q'
  sxaddpar, primary_header, 'i_to_u', i_to_u_xtalk, ' Crosstalk coefficient from I to U'
  sxaddpar, primary_header, 'q_to_u', q_to_u_xtalk, ' Crosstalk coefficient from Q to U'
  sxaddpar, primary_header, 'u_to_q', u_to_q_xtalk, ' Crosstalk coefficient from U to Q'

  ; N_EXT
  n_extensions = n_elements(headers_combine[0, *]) / 2
  sxaddpar, primary_header, 'N_EXT', n_extensions, $
            ' Number of extensions', after='EXTEND'
end
