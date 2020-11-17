; docformat = 'rst'

pro comp_make_elliptical_db, wave_type, config_filename, output_filename
  compile_opt strictarr
  @comp_config_common
  @comp_constants_common

  comp_configuration, config_filename=config_filename

  candidate_dates = comp_expand_date_expr(date_pattern, count=n_candidate_dates)
  if (n_candidate_dates eq 0) then begin
    t1 = systime(/seconds)
    mg_log, 'no days to process', name='comp', /error
    mg_log, 'total running time: %0.2f sec', t1 - t0, name='comp', /info
    mg_log, /quit
    return
  endif

  openw, lun, output_filename, /get_lun

  printf, lun, $
          'date', 'time', 'wave_name', 'wavelength', 'beam', 'occulter', $
          'ORADIUS1', 'ORAD1-2', 'ORADIUS2', 'ORAD2-2', 'OTILT1', 'OTILT2', $
          'ORADU1', 'ORADU1-2', 'ORADU2', 'ORADU2-2', 'OTILTU1', 'OTILTU2', $
          format='(%"%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s")'

  n_summary_exts = 3L

  for d = 0L, n_candidate_dates - 1L do begin
    date = candidate_dates[d]
    mg_log, 'processing %s', date, name='comp', /info
    comp_initialize, date

    fmt = '(%"%s.comp.flat.fts")'
    flat_filename = filepath(string(date, format=fmt), $
                             subdir=[date, 'level1'], $
                             root=process_basedir)
    if (~file_test(flat_filename, /regular)) then begin
      print, flat_filename, format='(%"  %s not found")'
      continue
    endif

    fits_open, flat_filename, fcb
    if (fcb.nextend - n_summary_exts le 0L) then begin
      fits_close, fcb
      print, '  no flats, skipping...'
      continue
    endif else print, fcb.nextend - n_summary_exts, format='(%"  %d extensions")'

    distcorr_radii = fltarr(fcb.nextend - n_summary_exts, 4)
    distcorr_tilt = fltarr(fcb.nextend - n_summary_exts, 2)
    uncorr_radii = fltarr(fcb.nextend - n_summary_exts, 4)
    uncorr_tilt = fltarr(fcb.nextend - n_summary_exts, 2)
    beam = lonarr(fcb.nextend - n_summary_exts)
    occulter = strarr(fcb.nextend - n_summary_exts)

    for e = 1L, fcb.nextend - n_summary_exts do begin
      fits_read, fcb, data, header, exten_no=e
      distcorr_radii[e - 1L, *] = [sxpar(header, 'ORADIUS1'), $
                                   sxpar(header, 'ORAD1-2'), $
                                   sxpar(header, 'ORADIUS2'), $
                                   sxpar(header, 'ORAD2-2')]
      distcorr_tilt[e - 1L, *] = [sxpar(header, 'OTILT1'), $
                                  sxpar(header, 'OTILT2')]
      uncorr_radii[e - 1L, *] = [sxpar(header, 'ORADU1'), $
                                 sxpar(header, 'ORADU1-2'), $
                                 sxpar(header, 'ORADU2'), $
                                 sxpar(header, 'ORADU2-2')]
      uncorr_tilt[e - 1L, *] = [sxpar(header, 'OTILTU1'), $
                                sxpar(header, 'OTILTU2')]
      beam[e - 1L] = sxpar(header, 'BEAM')
      occulter[e - 1L] = sxpar(header, 'OCCLTRID')
    endfor

    fits_read, fcb, times, times_header, exten_no=fcb.nextend - 2
    fits_read, fcb, wavelengths, wavelengths_header, exten_no=fcb.nextend - 1
    fits_close, fcb

    regions = [center1074, center1079, center1083]
    wave_names = ['1074', '1079', '1083']

    expanded_wavelengths = rebin(reform(wavelengths, 1, n_elements(wavelengths)), $
                                 n_elements(regions), $
                                 n_elements(wavelengths))
    regions = rebin(reform(regions, n_elements(regions), 1), $
                    n_elements(regions), $
                    n_elements(wavelengths))
    diffs = abs(expanded_wavelengths - regions)
    !null = min(diffs, dimension=1, closest_indices)

    indices = where(wave_names[closest_indices mod 3] eq wave_type, count)
    if (count eq 0L) then begin
      mg_log, 'no extensions of wave_type %s', wave_type, name='comp', /warn
      return
    endif

    distcorr_radii = distcorr_radii[indices, *]
    distcorr_tilt = distcorr_tilt[indices, *]
    uncorr_radii = uncorr_radii[indices, *]
    uncorr_tilt = uncorr_tilt[indices, *]
    times = times[indices]
    beam = beam[indices]
    wavelengths = wavelengths[indices]

    wave_name = string(wave_type, format='(%"%s")')

    for t = 0L, n_elements(times) - 1L do begin
      printf, lun, $
              date, times[t], wave_name, wavelengths[t], beam[t], occulter[t], $
              distcorr_radii[t, *], distcorr_tilt[t, *], $
              uncorr_radii[t, *], uncorr_tilt[t, *], $
              format='(%"%s, %f, %s, %f, %d, %s, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f")'
    endfor
  endfor

  done:
  free_lun, lun
end


; main-level example program

wave_type = '1074'
config_filename = filepath('comp.elliptic-newdist.cfg', $
                           subdir=['..', 'config'], $
                           root=mg_src_root())
output_filename = 'comp-2017-radii-newdist.csv'
comp_make_elliptical_db, wave_type, config_filename, output_filename

end
