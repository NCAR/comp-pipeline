; docformat = 'rst'

;+
; Update comp_eng table.
;
; :Params:
;   date : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :Keywords:
;   database : in, required, type=MGdbMySQL object
;     database to insert rows into
;   obsday_index : in, required, type=long
;     observing day as index into mlso_numfiles table
;
; :Author:
;   MLSO Software Team
;-
pro comp_eng_insert, date, wave_type, database=db, obsday_index=obsday_index
  compile_opt strictarr
  @comp_config_common

  l1_files = comp_find_l1_file(date, wave_type, /all, count=n_l1_files)

  if (n_l1_files gt 0L) then begin
    mg_log, 'inserting %d L1 files into %s nm comp_eng table...', $
            n_l1_files, wave_type, $
            name='comp', /info
  endif else begin
    mg_log, 'no L1 files to insert into %s nm comp_eng table...', $
            wave_type, $
            name='comp', /info
  endelse

  comp_sw_insert, date, database=db, obsday_index=obsday_index, sw_index=sw_id

  gbu_file = filepath(string(date, wave_type, format='(%"%s.comp.%s.gbu.log")'), $
                      subdir=[date, 'level1'], $
                      root=process_basedir)
  gbu = comp_read_gbu(gbu_file, count=n_gbu)

  for f = 0L, n_l1_files - 1L do begin
    fits_open, l1_files[f], fcb
    fits_read, fcb, data, primary_header, exten_no=0, /no_abort, message=msg, $
               /header_only
    n_extensions = fcb.nextend
    fits_read, fcb, data, header, exten_no=1, /no_abort, message=msg, $
               /header_only
    fits_close, fcb
    if (msg ne '') then message, msg

    date_obs = string(sxpar(primary_header, 'DATE-OBS'), $
                     sxpar(primary_header, 'TIME-OBS'), $
                     format='(%"%sT%s")')
    date_obs = comp_normalize_datetime(date_obs)

    obs_id = sxpar(primary_header, 'OBS_ID', count=n_obs_id)
    if (n_obs_id eq 0L) then obs_id = ''
    obs_plan = sxpar(primary_header, 'OBS_PLAN', count=n_obs_plan)
    if (n_obs_plan eq 0L) then obs_plan = ''

    focus = sxpar(primary_header, 'FOCUS')
    o1focus = sxpar(primary_header, 'O1FOCS', count=n_o1focus)
    o1focus = n_o1focus eq 0L ? 'NULL' : string(o1focus, format='(%"0.3f")')

    cover = strtrim(sxpar(primary_header, 'COVER'), 2)
    opal = strtrim(sxpar(primary_header, 'OPAL'), 2)

    xcenter1 = sxpar(primary_header, 'IXCNTER1')
    ycenter1 = sxpar(primary_header, 'IYCNTER1')
    radius1 = sxpar(primary_header, 'IRADIUS1')
    xcenter2 = sxpar(primary_header, 'IXCNTER2')
    ycenter2 = sxpar(primary_header, 'IYCNTER2')
    radius2 = sxpar(primary_header, 'IRADIUS2')

    uncor_xcenter1 = sxpar(primary_header, 'OXCNTRU1')
    uncor_ycenter1 = sxpar(primary_header, 'OYCNTRU1')
    uncor_radius1 = sxpar(primary_header, 'ORADU1')
    uncor_xcenter2 = sxpar(primary_header, 'OXCNTRU2')
    uncor_ycenter2 = sxpar(primary_header, 'OYCNTRU2')
    uncor_radius2 = sxpar(primary_header, 'ORADU2')

    overlap_angle = sxpar(primary_header, 'OVRLANG')
    post_angle = sxpar(primary_header, 'POSTANG')

    wavelength = comp_find_wave_type(sxpar(header, 'WAVELENG'))

    ntunes = sxpar(primary_header, 'NTUNES')
    pol_list = strtrim(sxpar(primary_header, 'POL_LIST'), 2)

    ; get GBU
    gbu_indices = where(l1_files[f] eq gbu.l1file, n_gbu_indices)
    if (n_gbu_indices eq 0L) then begin
      gbu_bitmask = 0L
    endif else begin
      gbu_bitmask = gbu[gbu_indices[0]].reason
    endelse

    exposure = sxpar(header, 'EXPOSURE')
    ndfilter = sxpar(header, 'NDFILTER')
    background = sxpar(header, 'BACKGRND')

    bodytemp = sxpar(header, 'BODYTEMP')
    basetemp = sxpar(header, 'BASETEMP')
    optrtemp = sxpar(header, 'OPTRTEMP')
    lcvr4temp = sxpar(header, 'LCVR4TMP')

    occulter_id = strtrim(sxpar(primary_header, 'OCC-ID'), 2)

    fields = [{name: 'file_name', type: '''%s'''}, $
              {name: 'date_obs', type: '''%s'''}, $
              {name: 'obs_day', type: '%d'}, $

              {name: 'focus', type: '%d'}, $
              {name: 'o1focus', type: '%s'}, $

              {name: 'obs_id', type: '''%s'''}, $
              {name: 'obs_plan', type: '''%s'''}, $

              {name: 'cover', type: '%d'}, $
              {name: 'opal', type: '%d'}, $

              {name: 'xcenter1', type: '%f'}, $
              {name: 'ycenter1', type: '%f'}, $
              {name: 'radius1', type: '%f'}, $
              {name: 'xcenter2', type: '%f'}, $
              {name: 'ycenter2', type: '%f'}, $
              {name: 'radius2', type: '%f'}, $

              {name: 'uncor_xcenter1', type: '%f'}, $
              {name: 'uncor_ycenter1', type: '%f'}, $
              {name: 'uncor_radius1', type: '%f'}, $
              {name: 'uncor_xcenter2', type: '%f'}, $
              {name: 'uncor_ycenter2', type: '%f'}, $
              {name: 'uncor_radius2', type: '%f'}, $

              {name: 'overlap_angle', type: '%f'}, $
              {name: 'post_angle', type: '%f'}, $

              {name: 'wavelength', type: '%f'}, $
              {name: 'ntunes', type: '%d'}, $
              {name: 'pol_list', type: '''%s'''}, $

              {name: 'nextensions', type: '%d'}, $

              {name: 'gbu_bitmask', type: '%d'}, $

              {name: 'exposure', type: '%f'}, $
              {name: 'nd', type: '%d'}, $
              {name: 'background', type: '%f'}, $

              {name: 'bodytemp', type: '%f'}, $
              {name: 'basetemp', type: '%f'}, $
              {name: 'optrtemp', type: '%f'}, $
              {name: 'lcvr4temp', type: '%f'}, $

              {name: 'occulter_id', type: '''%s'''}, $

              {name: 'comp_sw_id', type: '%d'}]
    sql_cmd = string(strjoin(fields.name, ', '), $
                     strjoin(fields.type, ', '), $
                     format='(%"insert into comp_eng (%s) values (%s)")')
    db->execute, sql_cmd, $
                 file_basename(l1_files[f]), $
                 date_obs, $
                 obsday_index, $

                 focus, $
                 o1focus, $

                 obs_id, $
                 obs_plan, $

                 cover, $
                 opal, $

                 xcenter1, $
                 ycenter1, $
                 radius1, $
                 xcenter2, $
                 ycenter2, $
                 radius2, $

                 uncor_xcenter1, $
                 uncor_ycenter1, $
                 uncor_radius1, $
                 uncor_xcenter2, $
                 uncor_ycenter2, $
                 uncor_radius2, $

                 overlap_angle, $
                 post_angle, $

                 wavelength, $
                 ntunes, $
                 pol_list, $

                 n_extensions, $

                 gbu_bitmask, $

                 exposure, $
                 ndfilter, $
                 background, $

                 bodytemp, $
                 basetemp, $
                 optrtemp, $
                 lcvr4temp, $

                 occulter_id, $

                 sw_id, $

                 status=status, $
                 error_message=error_message, $
                 sql_statement=final_sql_cmd, $
                 n_warnings=n_warnings
    if (status ne 0L) then begin
      mg_log, 'error inserting into comp_eng table', name='comp', /error
      mg_log, 'status: %d, error message: %s', status, error_message, $
              name='comp', /error
      mg_log, 'SQL command: %s', final_sql_cmd, name='comp', /error
    endif

    if (n_warnings gt 0L) then comp_db_log_warnings, database=db
  endfor

  done:
  mg_log, 'done', name='comp', /info
end
