; docformat = 'rst'

;+
; Update comp_sci table with results of pipeline run for the given day.
;
; :Params:
;   date : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :Keywords:
;   database : in, required, type=MGdbMySQL object
;     database connection
;   obsday_index : in, required, type=integer
;     index into mlso_numfiles database table
;
; :Author:
;   MLSO Software Team
;-
pro comp_sci_insert, date, wave_type, database=db, obsday_index=obsday_index
  compile_opt strictarr
  @comp_mask_constants_common
  @comp_config_common

  ; define full annulus
  min_radius = 1.05
  max_radius = 1.3
  n_radius_steps = 7L   ; every 0.05
  radii = (max_radius - min_radius) / (n_radius_steps - 1L) * findgen(n_radius_steps) $
            + min_radius

  filename_fmt = '(%"%s.comp.%s.mean.synoptic.fts.gz")'
  science_files = filepath(string(date, wave_type, format=filename_fmt), $
                           subdir=[date, 'level2'], $
                           root=process_basedir)
  n_science_files = n_elements(science_files)

  ; loop through science files
  for f = 0L, n_science_files - 1L do begin
    if (file_test(science_files[f], /regular)) then begin
      mg_log, 'inserting %s into comp_sci table', file_basename(science_files[f]), $
              name='comp', /info
    endif else begin
      mg_log, 'skipping %s', science_files[f], name='comp', /warn
      continue
    endelse

    fits_open, science_files[f], fcb
    fits_read, fcb, data, primary_header, exten_no=0, /no_abort, message=msg
    if (msg ne '') then message, msg

    fits_read, fcb, intensity_image, intensity_header, exten_no=3
    fits_read, fcb, continuum_image, continuum_header, exten_no=fcb.nextend - 2L

    fits_close, fcb

    date_obs = string(sxpar(primary_header, 'DATE-OBS'), $
                     sxpar(primary_header, 'TIME-OBS'), $
                     format='(%"%sT%s")')
    date_obs = comp_normalize_datetime(date_obs)

    year   = long(strmid(date_obs,  0, 4))
    month  = long(strmid(date_obs,  5, 2))
    day    = long(strmid(date_obs,  8, 2))
    hour   = long(strmid(date_obs, 11, 2))
    minute = long(strmid(date_obs, 14, 2))
    second = long(strmid(date_obs, 17, 2))

    fhour = hour + minute / 60.0 + second / 60.0 / 60.0
    sun, year, month, day, fhour, sd=rsun, pa=pangle, la=bangle

    sun_pixels = rsun / plate_scale

    cx = sxpar(primary_header, 'CRPIX1') - 1.0   ; convert from FITS convention to
    cy = sxpar(primary_header, 'CRPIX2') - 1.0   ; IDL convention

    intensity = comp_extract_radial_values(intensity_image, radii, sun_pixels, $
                                           cx=cx, cy=cy, $
                                           standard_deviation=intensity_stddev)
    continuum = comp_extract_radial_values(continuum_image, radii, sun_pixels, $
                                           cx=cx, cy=cy, $
                                           standard_deviation=continuum_stddev)

    east_intensity = comp_extract_radial_values(intensity_image, radii, sun_pixels, $
                                                limb='east', $
                                                cx=cx, cy=cy, $
                                                standard_deviation=east_intensity_stddev)
    west_intensity = comp_extract_radial_values(intensity_image, radii, sun_pixels, $
                                                limb='west', $
                                                cx=cx, cy=cy, $
                                                standard_deviation=west_intensity_stddev)

    r11_intensity = comp_annulus_gridmeans(intensity_image, 1.1, sun_pixels, $
                                           nbins=720, width=0.02)
    r11_continuum = comp_annulus_gridmeans(continuum_image, 1.1, sun_pixels, $
                                           nbins=720, width=0.02)
    r12_intensity = comp_annulus_gridmeans(intensity_image, 1.2, sun_pixels, $
                                           nbins=720, width=0.02)

    r110_intensity_mean = comp_annulus_mean(intensity_image, 1.08, 1.13, sun_pixels)
    r110_background_mean = comp_annulus_mean(continuum_image, 1.08, 1.13, sun_pixels)

    ; insert into comp_sci table
    fields = [{name: 'file_name', type: '''%s'''}, $
              {name: 'date_obs', type: '''%s'''}, $
              {name: 'obs_day', type: '%d'}, $
              {name: 'wavetype', type: '%s'}, $

              {name: 'r110_intensity_mean', type: '%8.4f'}, $
              {name: 'r110_background_mean', type: '%8.4f'}, $

              {name: 'intensity', type: '''%s'''}, $
              {name: 'intensity_stddev', type: '''%s'''}, $
              {name: 'continuum', type: '''%s'''}, $
              {name: 'continuum_stddev', type: '''%s'''}, $
              {name: 'east_intensity', type: '''%s'''}, $
              {name: 'east_intensity_stddev', type: '''%s'''}, $
              {name: 'west_intensity', type: '''%s'''}, $
              {name: 'west_intensity_stddev', type: '''%s'''}, $

              {name: 'r11_intensity', type: '''%s'''}, $
              {name: 'r12_intensity', type: '''%s'''}, $
              {name: 'r11_continuum', type: '''%s'''}]
    sql_cmd = string(strjoin(fields.name, ', '), $
                     strjoin(fields.type, ', '), $
                     format='(%"insert into comp_sci (%s) values (%s)")')
    db->execute, sql_cmd, $
                 file_basename(science_files[f], '.gz'), $
                 date_obs, $
                 obsday_index, $
                 wave_type, $

                 r110_intensity_mean, $
                 r110_background_mean, $

                 db->escape_string(intensity), $
                 db->escape_string(intensity_stddev), $
                 db->escape_string(continuum), $
                 db->escape_string(continuum_stddev), $
                 db->escape_string(east_intensity), $
                 db->escape_string(east_intensity_stddev), $
                 db->escape_string(west_intensity), $
                 db->escape_string(west_intensity_stddev), $

                 db->escape_string(r11_intensity), $
                 db->escape_string(r12_intensity), $
                 db->escape_string(r11_continuum), $

                 status=status, $
                 error_message=error_message, $
                 sql_statement=final_sql_cmd, $
                 n_warnings=n_warnings
  endfor

  done:
  mg_log, 'done', name='comp', /info
end

; main-level example program
@comp_config_common

wave_type = '1074'
date = '20180101'

config_filename = filepath('comp.latest.cfg', $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())
comp_initialize, date
comp_configuration, config_filename=config_filename
comp_update_configuration, date

obsday_index = mlso_obsday_insert(date, $
                                  database_config_filename, $
                                  database_config_section, $
                                  database=db, $
                                  status=status, $
                                  log_name='comp')

comp_sci_insert, date, wave_type, database=db, obsday_index=obsday_index

obj_destroy, db

end
