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

  ; define annulus
  min_radius = 1.05
  max_radius = 1.3
  n_radius_steps = 7L   ; every 0.05
  radii = (max_radius - min_radius) / (n_radius_steps - 1L) * findgen(n_radius_steps) $
            + min_radius

  ; find L1 files
  l1_files = comp_find_l1_file(date, wave_type, /all, count=n_l1_files)
  bkg_files = comp_find_l1_file(date, wave_type, /all, count=n_bkg_files, /background)

  if (n_l1_files gt 0L) then begin
    mg_log, 'inserting %d rows into %s nm comp_sci table...', $
            n_l1_files, wave_type, $
            name='comp', /info
  endif else begin
    mg_log, 'no L1 files to insert into %s nm comp_sci table', $
            wave_type, $
            name='comp', /info
    goto, done
  endelse

  produce_continuum = n_bkg_files gt 0L

  ; just choosing the 20th L1 file right now (or the last file, if less than 20
  ; L1 files)
  science_files = l1_files[(n_l1_files < 20L) - 1L]
  if (produce_continuum) then science_bkg_files = bkg_files[(n_l1_files < 20L) - 1L]
  n_science_files = n_elements(science_files)

  ; loop through science files
  for f = 0L, n_science_files - 1L do begin
    fits_open, science_files[f], fcb
    fits_read, fcb, data, primary_header, exten_no=0, /no_abort, message=msg
    fits_close, fcb
    if (msg ne '') then message, msg

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

    comp_extract_intensity_cube, science_files[f], $
                                 images=intensity_images, $
                                 pol_state='I'
    if (produce_continuum) then begin
      comp_extract_intensity_cube, science_bkg_files[f], $
                                   images=continuum_images, $
                                   /background, $
                                   pol_state='I'
    endif

    intensity_image = mean(intensity_images, dimension=3)
    if (produce_continuum) then continuum_image = mean(continuum_images, dimension=3)

    intensity = comp_extract_radial_values(intensity_image, radii, sun_pixels, $
                                           cx=cx, cy=cy, $
                                           standard_deviation=intensity_stddev)
    if (produce_continuum) then begin
      continuum = comp_extract_radial_values(continuum_image, radii, sun_pixels, $
                                             cx=cx, cy=cy, $
                                             standard_deviation=continuum_stddev)
    endif else begin
      continuum = fltarr(n_radius_steps)
      continuum_stddev = fltarr(n_radius_steps)
    endelse

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
    if (produce_continuum) then begin
      r11_continuum = comp_annulus_gridmeans(continuum_image, 1.1, sun_pixels, $
                                             nbins=720, width=0.02)
    endif else begin
      r11_continuum = fltarr(720)
    endelse
    r12_intensity = comp_annulus_gridmeans(intensity_image, 1.2, sun_pixels, $
                                           nbins=720, width=0.02)

    ; insert into comp_sci table
    fields = [{name: 'file_name', type: '''%s'''}, $
              {name: 'date_obs', type: '''%s'''}, $
              {name: 'obs_day', type: '%d'}, $

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
    if (status ne 0L) then begin
      mg_log, 'error inserting into comp_sci table', name='comp', /error
      mg_log, 'status: %d, error message: %s', status, error_message, $
              name='comp', /error
      mg_log, 'SQL command: %s', final_sql_cmd, name='comp', /error
    endif

    if (n_warnings gt 0L) then comp_db_log_warnings, database=db
  endfor

  done:
  mg_log, 'done', name='comp', /info
end

; main-level example program

wave_type = '1083'

date = '20170213'
config_filename = filepath('comp.elliptic.cfg', $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())
comp_initialize, date
comp_configuration, config_filename=config_filename

l1_files = comp_find_l1_file(date, wave_type, /all, count=n_l1_files)
bkg_files = comp_find_l1_file(date, wave_type, /all, count=n_bkg_files, /background)
help, l1_files, n_l1_files
help, bkg_files, n_bkg_files

end
