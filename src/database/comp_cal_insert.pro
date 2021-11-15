; docformat = 'rst'

;+
; Update comp_cal table.
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
pro comp_cal_insert, date, database=db, obsday_index=obsday_index
  compile_opt strictarr
  @comp_config_common

  level_id = comp_get_level_id('L1', database=db)

  ; find cal, dark, and flat files from listings:
  ;   - YYYYMMDD.comp.cal.files.txt
  ;   - YYYYMMDD.comp.dark.files.txt
  ;   - YYYYMMDD.comp.opal.files.txt
  types = ['cal', 'dark', 'opal']
  fmt = string(date, format='(%"%s.comp.%%s.files.txt")')
  catalog_basenames = string(types, format='(%"' + fmt + '")')
  catalog_filenames = filepath(catalog_basenames, $
                               subdir=[date, 'level1'], $
                               root=process_basedir)

  flats_basename = string(date, format='(%"%s.comp.flat.fts")')
  flats_filename = filepath(flats_basename, $
                            subdir=[date, 'level1'], $
                            root=process_basedir)
  if (file_test(flats_filename, /regular)) then begin
    fits_open, flats_filename, flats_fcb

    n_flats = flats_fcb.nextend - 3L  ; last 3 extensions are time, wavelength, exposure
    flats_rawnames = strarr(n_flats)
    flats_beam = bytarr(n_flats)
    fits_read, flats_fcb, data, flat_ext_header, exten_no=1, /header_only
    flats_headers = strarr(n_flats, n_elements(flat_ext_header))

    for e = 1L, n_flats do begin
      fits_read, flats_fcb, data, flat_ext_header, exten_no=e, /header_only
      flats_rawnames[e - 1L] = sxpar(flat_ext_header, 'FILENAME')
      flats_beam[e - 1L] = sxpar(flat_ext_header, 'BEAM')
      flats_headers[e - 1L, *] = flat_ext_header
    endfor
    fits_close, flats_fcb
  endif

  for c = 0L, n_elements(types) - 1L do begin
    if (~file_test(catalog_filenames[c], /regular)) then continue

    n_files = file_lines(catalog_filenames[c])

    if (n_files eq 0L) then begin
      mg_log, 'no %s files to insert into comp_cal table', $
              types[c], $
              name='comp', /info
      continue
    endif else begin
      mg_log, 'inserting %d %s files into comp_cal table', $
              n_files, types[c], $
              name='comp', /info
    endelse

    files = strarr(n_files)
    openr, lun, catalog_filenames[c], /get_lun
    readf, lun, files
    free_lun, lun

    cal_basenames = strmid(files, 0, 19)
    for f = 0L, n_files - 1L do begin
      catch, error
      if (error ne 0L) then begin
        mg_log, '%s', strcompress(strjoin(strtrim(!error_state.msg, 2), ', ')), $
                name='comp', /warn
        mg_log, 'skipping %s', cal_basenames[f], name='comp', /warn
        continue
      endif
      filename = filepath(cal_basenames[f], subdir=date, root=raw_basedir)

      comp_read_data, filename, images, headers, primary_header

      dateobs = sxpar(primary_header, 'DATE_OBS')
      timeobs = sxpar(primary_header, 'TIME_OBS')

      date_tokens = strsplit(dateobs, '/', /extract)
      year = long(date_tokens[2])
      month = long(date_tokens[0])
      day = long(date_tokens[1])

      time_tokens = strsplit(timeobs, ' :', /extract)
      hour = long(time_tokens[0])
      minute = long(time_tokens[1])
      second = long(time_tokens[2])

      date_obs = string(year, month, day, hour, minute, second, $
                        format='(%"%04d-%02d-%02dT%02d:%02d:%02d")')

      ntunes = sxpar(primary_header, 'NTUNES')
      cover = sxpar(primary_header, 'COVER')
      opal = sxpar(primary_header, 'OPAL')
      polangle = sxpar(primary_header, 'POLANGLE')
      polarizer = sxpar(primary_header, 'POLARIZR')
      retarder = strtrim(sxpar(primary_header, 'RETARDER'), 2)

      occulter_id = strtrim(sxpar(primary_header, 'OCCULTER'), 2)

      wavelength = comp_find_wave_type(sxpar(headers[*, 0], 'WAVELENG'))
      exposure = sxpar(headers[*, 0], 'EXPOSURE')
      ndfilter = sxpar(headers[*, 0], 'NDFILTER')

      ; get centering information
      ext_indices = where(flats_rawnames eq cal_basenames[f], count)

      fheader = reform(flats_headers[ext_indices[0], *])  ; just using 1st match

      xcenter1 = sxpar(fheader, 'OXCNTER1')
      ycenter1 = sxpar(fheader, 'OYCNTER1')
      radius1 = sxpar(fheader, 'ORADIUS1')
      xcenter2 = sxpar(fheader, 'OXCNTER2')
      ycenter2 = sxpar(fheader, 'OYCNTER2')
      radius2 = sxpar(fheader, 'ORADIUS2')

      uncor_xcenter1 = sxpar(fheader, 'OXCNTRU1')
      uncor_ycenter1 = sxpar(fheader, 'OYCNTRU1')
      uncor_radius1 = sxpar(fheader, 'ORADU1')
      uncor_xcenter2 = sxpar(fheader, 'OXCNTRU2')
      uncor_ycenter2 = sxpar(fheader, 'OYCNTRU2')
      uncor_radius2 = sxpar(fheader, 'ORADU2')

      ; compute medians of dark corrected annulus
      if (cover eq 1 && opal eq 1) then begin
        time = comp_extract_time(headers)
        dark = comp_dark_interp(date, time, exposure)

        comp_inventory_header, headers, beam, wave, pol, type, expose, $
                               cover, cal_pol, cal_ret

        center_wavelength = comp_find_wave_type(wave)

        minus_flat_indices = where(abs(wave - center_wavelength) lt 0.1 and beam eq -1, $
                                   n_minus_flat)
        plus_flat_indices = where(abs(wave - center_wavelength) lt 0.1 and beam eq 1, $
                                  n_plus_flat)

        if (n_minus_flat eq 0L) then begin
          mg_log, 'no flats with -1 beam state found, skipping', name='comp', /warn
          continue
        endif
        if (n_plus_flat eq 0L) then begin
          mg_log, 'no flats with +1 beam state found, skipping', name='comp', /warn
          continue
        endif

        minus_image = mean(images[*, *, minus_flat_indices], dimension=3)
        plus_image = mean(images[*, *, plus_flat_indices], dimension=3)

        mask1 = comp_mask_1024_1(fheader, margin=0.0)
        mask2 = comp_mask_1024_2(fheader, margin=0.0)

        mask1_indices = where(mask1, count)
        mask2_indices = where(mask2, count)

        median_int_continuum_beam0 = median(minus_image[mask2_indices])
        median_int_linecenter_beam0 = median(minus_image[mask1_indices])

        median_int_continuum_beam1 = median(plus_image[mask1_indices])
        median_int_linecenter_beam1 = median(plus_image[mask2_indices])

      endif else begin   ; is not useful for darks
        median_int_continuum_beam0 = 0.0
        median_int_linecenter_beam0 = 0.0
        median_int_continuum_beam1 = 0.0
        median_int_linecenter_beam1 = 0.0
      endelse


      fields = [{name: 'file_name', type: '''%s'''}, $
                {name: 'date_obs', type: '''%s'''}, $
                {name: 'obs_day', type: '%d'}, $

                {name: 'wavelength', type: '%f'}, $
                {name: 'n_points', type: '%d'}, $
                {name: 'level', type: '%s'}, $
                {name: 'exposure', type: '%f'}, $
                {name: 'nd', type: '%d'}, $

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

                {name: 'median_int_continuum_beam0', type: '%f'}, $
                {name: 'median_int_linecenter_beam0', type: '%f'}, $

                {name: 'median_int_continuum_beam1', type: '%f'}, $
                {name: 'median_int_linecenter_beam1', type: '%f'}, $

                {name: 'occulter_id', type: '''%s'''}]
      sql_cmd = string(strjoin(fields.name, ', '), $
                       strjoin(fields.type, ', '), $
                       format='(%"insert into comp_cal (%s) values (%s)")')
      db->execute, sql_cmd, $
                   cal_basenames[f], $
                   date_obs, $
                   obsday_index, $

                   wavelength, $
                   ntunes, $
                   level_id, $
                   exposure, $
                   ndfilter, $

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

                   median_int_continuum_beam0, $
                   median_int_linecenter_beam0, $
                   median_int_continuum_beam1, $
                   median_int_linecenter_beam1, $

                   occulter_id, $

                   status=status
    endfor
  endfor

  done:
  mg_log, 'done', name='comp', /info
end


; main-level example program

@comp_config_common

date = '20130115'
comp_initialize, date
config_filename = filepath('comp.db.cfg', $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())
comp_configuration, config_filename=config_filename

obsday_index = mlso_obsday_insert(date, $
                                  database_config_filename, $
                                  database_config_section, $
                                  database=db, status=status, log_name='comp')

comp_db_clearday, database=db, obsday_index=obsday_index

comp_cal_insert, date, database=db, obsday_index=obsday_index
obj_destroy, db

end
