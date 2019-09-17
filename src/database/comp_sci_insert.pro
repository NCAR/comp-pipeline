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

  ; TODO: define annulus
  min_annulus_radius = 1.08
  min_annulus_radius = 3.0

  ; find L1 files
  l1_files = comp_find_l1_files(date, wave_type, /all, count=n_l1_files)

  if (n_l1_files gt 0L) then begin
    mg_log, 'inserting row into comp_sci table...', n_l1_files, $
            name='comp', /info
  endif else begin
    mg_log, 'no L1 files to insert into comp_sci table', name='comp', /info
    goto, done
  endelse

  ; just choosing the 20th L1 file right now (or the last file, if less than 20
  ; L1 files)
  science_files = l1_files[(n_l1_files < 20L) - 1L]
  n_science_files = n_elements(science_files)
  
  ; angles for full circle in radians
  theta = findgen(360) * !dtor

  ; loop through science files
  for f = 0L, n_science_files - 1L do begin
    fits_open, science_files[f], fcb
    fits_read, fcb, data, primary_header, exten_no=0, /no_abort, message=msg
    fits_close, fcb
    if (msg ne '') then message, msg

    date_obs = string(sxpar(primary_header, 'DATE-OBS'), $
                     sxpar(primary_header, 'TIME-OBS'), $
                     format='(%"%sT%s")')

    year   = long(strmid(date_obs,  0, 4))
    month  = long(strmid(date_obs,  5, 2))
    day    = long(strmid(date_obs,  8, 2))
    hour   = long(strmid(date_obs, 11, 2))
    minute = long(strmid(date_obs, 14, 2))
    second = long(strmid(date_obs, 17, 2))

    fhour = hour + minute / 60.0 + second / 60.0 / 60.0
    sun, year, month, day, fhour, sd=rsun, pa=pangle, la=bangle

    sun_pixels = rsun / run->epoch('plate_scale')

    ; TODO: calculate total{i,q,u}, intensity{,_stdev}, {q,u}{,_stddev},
    ; r{108,13}{i,l}, and r{108,13}radazi
    cx = sxpar(primary_header, 'CRPIX1') - 1.0   ; convert from FITS convention to
    cy = sxpar(primary_header, 'CRPIX2') - 1.0   ; IDL convention

    x = (rebin(reform(findgen(nx), nx, 1), nx, ny) - cx) / sun_pixels
    y = (rebin(reform(findgen(ny), 1, ny), nx, ny) - cy) / sun_pixels
    d = sqrt(x^2 + y^2)
    annulus = where(d gt min_annulus_radius and d lt max_annulus_radius, count)

    comp_extract_intensity_cube, science_files[f], $
                                 images=intensity_images, $
                                 pol_state='I'
    comp_extract_intensity_cube, science_files[f], $
                                 images=q_images, $
                                 pol_state='Q'
    comp_extract_intensity_cube, science_files[f], $
                                 images=u_images, $
                                 pol_state='U'

    ; insert into comp_sci table
    fields = [{name: 'file_name', type: '''%s'''}, $
              {name: 'date_obs', type: '''%s'''}, $
              {name: 'obs_day', type: '%d'}, $
              {name: 'totali', type: '%f'}, $
              {name: 'totalq', type: '%f'}, $
              {name: 'totalu', type: '%f'}, $
              {name: 'intensity', type: '''%s'''}, $
              {name: 'intensity_stddev', type: '''%s'''}, $
              {name: 'q', type: '''%s'''}, $
              {name: 'q_stddev', type: '''%s'''}, $
              {name: 'u', type: '''%s'''}, $
              {name: 'u_stddev', type: '''%s'''}, $
              {name: 'r108i', type: '''%s'''}, $
              {name: 'r13i', type: '''%s'''}, $
              {name: 'r108l', type: '''%s'''}, $
              {name: 'r13l', type: '''%s'''}, $
              {name: 'r108radazi', type: '''%s'''}, $
              {name: 'r13radazi', type: '''%s'''}]
    sql_cmd = string(strjoin(fields.name, ', '), $
                     strjoin(fields.type, ', '), $
                     format='(%"insert into comp_sci (%s) values (%s)")')
    db->execute, sql_cmd, $
                 file_basename(science_files[f], '.gz'), $
                 date_obs, $
                 obsday_index, $
                 total_i, total_q, total_u, $
                 db->escape_string(intensity), $
                 db->escape_string(intensity_stdev), $
                 db->escape_string(q), $
                 db->escape_string(q_stddev), $
                 db->escape_string(u), $
                 db->escape_string(u_stddev), $
                 db->escape_string(r108i), $
                 db->escape_string(r13i), $
                 db->escape_string(r108l), $
                 db->escape_stringr(r13l), $
                 db->escape_string(r108radazi), $
                 db->escape_string(r13radazi), $
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
