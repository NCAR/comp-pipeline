; docformat = 'rst'

;+
; Update comp_dynamics and comp_file tables with results of pipeline run for
; the given day.
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
pro comp_dynamics_insert, date, wave_type, $
                          database=db, obsday_index=obsday_index
  compile_opt strictarr
  @comp_config_common

  basename = string(wave_type, format='(%"*.comp.%s.dynamics.fts*")')
  dynamics_files = file_search(filepath(basename, $
                                        subdir=[date, 'level2'], $
                                        root=process_basedir), $
                               count=n_dynamics_files)

  if (n_dynamics_files gt 0L) then begin
    mg_log, 'inserting %d rows into %s nm comp_dynamics table...', $
            n_dynamics_files, wave_type, $
            name='comp', /info
  endif else begin
    mg_log, 'no dynamics files to insert into %s nm comp_dynamics table', $
            wave_type, $
            name='comp', /info
    goto, done
  endelse

  ; get dynamics product type from mlso_producttype
  producttype = comp_get_producttype_id('dynamics', database=db)

  ; get file type from mlso_filetype
  filetype = comp_get_filetype_id('fits', database=db)

  ; get level from comp_level
  level = comp_get_level_id('L2', database=db)

  quality = 100

  ; loop through dynamics files to insert into comp_dynamics and comp_img
  for f = 0L, n_dynamics_files - 1L do begin
    mg_log, '%d/%d: inserting %s', $
            f + 1, n_dynamics_files, file_basename(dynamics_files[f]), $
            name='comp', /debug
    fits_open, dynamics_files[f], fcb
    fits_read, fcb, data, primary_header, exten_no=0, /no_abort, message=msg
    fits_read, fcb, intensity, intensity_header, exten_no=1, /no_abort, message=msg
    fits_read, fcb, velocity, velocity_header, exten_no=3, /no_abort, message=msg
    fits_close, fcb
    if (msg ne '') then message, msg

    date_obs = string(sxpar(primary_header, 'DATE-OBS'), $
                      sxpar(primary_header, 'TIME-OBS'), $
                      format='(%"%sT%s")')
    date_obs = comp_normalize_datetime(date_obs)

    dims = size(velocity, /dimensions)
    mask = comp_l2_mask(primary_header)
    mask_indices = where(mask, /null)

    intensity_max = max(intensity[mask_indices], /nan)
    doppler_min = min(velocity[mask_indices], max=doppler_max, /nan)

    doppler_mean = sxpar(velocity_header, 'RSTWVL')
    doppler_east_median = sxpar(velocity_header, 'ERSTWVL')
    doppler_west_median = sxpar(velocity_header, 'WRSTWVL')
    doppler_median = sxpar(velocity_header, 'RSTWVL2')
    doppler_east_mean = sxpar(velocity_header, 'ERSTWVL2')
    doppler_west_mean = sxpar(velocity_header, 'WRSTWVL2')
    doppler_device_east_median = sxpar(velocity_header, 'ERSTWVLD')
    doppler_device_west_median = sxpar(velocity_header, 'WRSTWVLD')

    ; insert into comp_dynamics table
    fields = [{name: 'file_name', type: '''%s'''}, $
              {name: 'date_obs', type: '''%s'''}, $
              {name: 'obs_day', type: '%d'}, $

              {name: 'intensity_max', type: '%f'}, $
              {name: 'doppler_min', type: '%f'}, $
              {name: 'doppler_max', type: '%f'}, $

              {name: 'doppler_mean', type: '%f'}, $
              {name: 'doppler_east_median', type: '%f'}, $
              {name: 'doppler_west_median', type: '%f'}, $
              {name: 'doppler_median', type: '%f'}, $
              {name: 'doppler_east_mean', type: '%f'}, $
              {name: 'doppler_west_mean', type: '%f'}, $
              {name: 'doppler_device_east_median', type: '%f'}, $
              {name: 'doppler_device_west_median', type: '%f'}]
    sql_cmd = string(strjoin(fields.name, ', '), $
                     strjoin(fields.type, ', '), $
                     format='(%"insert into comp_dynamics (%s) values (%s)")')
    db->execute, sql_cmd, $
                 file_basename(dynamics_files[f], '.gz'), $
                 date_obs, $
                 obsday_index, $

                 intensity_max, $
                 doppler_min, $
                 doppler_max, $

                 doppler_mean, $
                 doppler_east_median, $
                 doppler_west_median, $
                 doppler_median, $
                 doppler_east_mean, $
                 doppler_west_mean, $
                 doppler_device_east_median, $
                 doppler_device_west_median, $

                 status=status, $
                 error_message=error_message, $
                 sql_statement=sql_query
    if (status ne 0L) then begin
      mg_log, 'comp_file status: %d', status, name='comp', /warn
      mg_log, 'error: %s', error_message, name='comp', /warn
      mg_log, 'sql_query: %s', sql_query, name='comp', /warn
    endif

    carrington_rotation = sxpar(primary_header, 'CARR_ROT')
    pol_list = strtrim(sxpar(primary_header, 'POL_LIST'), 2)

    ; insert into comp_file table
    fields = [{name: 'file_name', type: '''%s'''}, $
              {name: 'date_obs', type: '''%s'''}, $
              {name: 'obs_day', type: '%d'}, $

              {name: 'carrington_rotation', type: '%d'}, $
              {name: 'level', type: '%d'}, $
              {name: 'producttype', type: '%d'}, $
              {name: 'filetype', type: '%d'}, $
              {name: 'quality', type: '%d'}, $
              {name: 'pol_list', type: '''%s'''}, $
              {name: 'wavetype', type: '''%s'''}];, $
              ;{name: 'ntunes', type: '%d'}]
    sql_cmd = string(strjoin(fields.name, ', '), $
                     strjoin(fields.type, ', '), $
                     format='(%"insert into comp_file (%s) values (%s)")')

    db->execute, sql_cmd, $
                 file_basename(dynamics_files[f], '.gz'), $
                 date_obs, $
                 obsday_index, $

                 carrington_rotation, $
                 level, $
                 producttype, $
                 filetype, $
                 quality, $
                 pol_list, $
                 wave_type, $
                 ;ntunes, $

                 status=status, $
                 error_message=error_message, $
                 sql_statement=sql_query
    if (status ne 0L) then begin
      mg_log, 'comp_file status: %d', status, name='comp', /warn
      mg_log, 'error: %s', error_message, name='comp', /warn
      mg_log, 'sql_query: %s', sql_query, name='comp', /warn
    endif
  endfor

  done:
  mg_log, 'done', name='comp', /info
end


; main-level example program
@comp_config_common

date = '20180101'

comp_initialize, date
config_filename = filepath('comp.db.cfg', $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())
comp_configuration, config_filename=config_filename

; get database connection
db = compdbmysql()
db->connect, config_filename=database_config_filename, $
             config_section=database_config_section, $
             status=status, error_message=error_message

; get obs day index
obsday_index = mlso_obsday_insert(date, database_config_filename, database_config_section, $
                                  database=db, status=status)

print, date, obsday_index, format='Obsday index for %s is %d'

comp_dynamics_insert, date, '1074', $
                      database=db, obsday_index=obsday_index
comp_dynamics_insert, date, '1079', $
                      database=db, obsday_index=obsday_index

obj_destroy, db

end
