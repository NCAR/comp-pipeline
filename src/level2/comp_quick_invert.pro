; docformat = 'rst'

;+
; Procedure to perform an approximate 'quick' inversion of some parameters from
; the comp averaged Level_2 data file. This routine reads the
; YYYYMMDD.comp.wwww.median.fts which was computed with `comp_average`, where
; "wwww" is the wave_type and "YYYYMMDD" is the date.
;
; The output is a FITS file written to the process directory named
; date_dir.comp.wwww.quick_invert.fts where "wwww" is the wave_type. The output
; FITS file contains extensions with the following images:
;
;   I - approximated by the Stokes I image nearest line center
;   Q - approximated by the Stokes Q image rearest line center 
;   U - approximated by the Stokes U image rearest line center
;   Linear Polarization - computed as sqrt( Q^2 + U^2)
;   Azimuth - computed as 0.5 atan( U / Q)
;   Radial Azimuth
;   Doppler Velocity - computed from the analytic gaussian fit of the intensity
;                      of the three images nearest line center
;   Line Width - computed from the analytic gaussian fit of the intensity of
;                the three images nearest line center
;
; :Examples:
;   For example, call like::
;
;     comp_quick_invert, '20110504', '1074', /synthetic
;     comp_quick_invert, '20130915', '1074'
;
; :Uses:
;   comp_simulate_common, comp_constants_common, comp_config_common,
;   comp_azimuth, comp_analytic_gauss_fit2, fits_open, fits_read, fits_write,
;   fits_close, sxpar, sxaddpar, sxdelpar, mg_log
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :Keywords:
;   synthetic : in, optional, type=boolean
;     process synthetic data set (not typically done)
;   error : out, optional, type=long
;     set to a named variable to return the error status of the routine, 0 for
;     success, anything else for failure
;   synoptic : in, optional, type=boolean
;     set to use synoptic file instead of waves file
;   method : in, optional, type=string, default='median'
;     set to 'mean' or 'median' to indicate the average files to use
;
; :Author:
;   MLSO Software Team
;
; :History:
;   removed gzip    Oct 1 2014  GdT
;   removed copy_file of intensity fits to archive_dir  Oct 1 2014  GdT
;   see git log for recent changes
;-
pro comp_quick_invert, date_dir, wave_type, $
                       synthetic=synthetic, error=error, synoptic=synoptic, $
                       method=method
  compile_opt idl2
  @comp_simulate_common
  @comp_constants_common
  @comp_config_common

  mg_log, 'quick invert %s (%s) [%s]', $
          wave_type, method, $
          keyword_set(synthetic) $
            ? 'synthetic' $
            : (keyword_set(synoptic) ? 'synoptic' : 'waves'), $
          name='comp', /info

  ; establish error handler for a crash in this routine
  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    mg_log, /last_error, name='comp'
    return
  endif

  l2_process_dir = filepath('', subdir=[date_dir, 'level2'], root=process_basedir)
  cd, l2_process_dir

  _method = n_elements(method) eq 0L ? 'median' : method
  type = keyword_set(synoptic) ? 'synoptic' : 'waves'

  ; create filename and open input FITS file
  if (keyword_set(synthetic)) then begin
    file = string(date_dir, wave_type, format='(%"%s.comp.%s.synthetic.fts.gz")')
  endif else begin
    file = string(date_dir, wave_type, _method, type, format='(%"%s.comp.%s.%s.%s.fts.gz")')
  endelse

  if (~file_test(file) || file_test(file, /zero_length)) then begin
    mg_log, '%s does not exist, exiting', file, name='comp', /warn
    return
  endif

  fits_open, file, fcb
  n = fcb.nextend

  comp_inventory, fcb, beam, wavelengths, error=error
  if (error gt 0L) then begin
    mg_log, 'error reading %s, quitting', file, name='comp', /error
    goto, done
  endif

  ; copy the primary header from the median file to the output file
  fits_read, fcb, d, primary_header, /header_only, exten_no=0

  sxdelpar, primary_header, 'OBS_PLAN'
  sxdelpar, primary_header, 'OBS_ID'

  ntune = sxpar(primary_header, 'NTUNE', count=nrecords)
  if (nrecords eq 0L) then ntune = sxpar(primary_header, 'NTUNES')

  nstokes = n / ntune - 1L   ; don't count BKG

  ; find standard 3 pt wavelength indices
  wave_indices = comp_3pt_indices(wave_type, wavelengths, error=error)
  if (error ne 0L) then begin
    mg_log, 'standard 3pt wavelengths not found in %s', $
            file_basename(file), name='comp', /error
  endif

  ; read data
  comp_obs = fltarr(nx, nx, nstokes, ntune)
  wave = fltarr(ntune)

  e = 1
  for is = 0L, nstokes - 1L do begin
    for iw = 0L, ntune - 1L do begin
      fits_read, fcb, dat, h, exten_no=e
      comp_obs[*, *, is, iw] = dat
      wave[iw] = sxpar(h, 'WAVELENG')
      ++e
    endfor
  endfor

  ; use header for center wavelength for I as template
  fits_read, fcb, dat, header, exten_no=ntune / 2

  fits_close, fcb

  sxaddpar, primary_header, 'N_EXT', 8, /savecomment

  case wave_type of
    '1074': rest = double(center1074)
    '1079': rest = double(center1079)
    '1083': rest = double(center1083)
  endcase
  c = 299792.458D

  sxaddpar, primary_header, 'METHOD', _method, $
            'Input file type used for quick invert'

  ; update version
  comp_l2_update_version, primary_header

  ; compute parameters
  i = comp_obs[*, *, 0, wave_indices[1]]
  q = comp_obs[*, *, 1, wave_indices[1]]
  u = comp_obs[*, *, 2, wave_indices[1]]

  zero = where(i le 0, count)
  if (count eq 0) then mg_log, 'no zeros', name='comp', /warn

  ; compute azimuth and adjust for p-angle, correct azimuth for quadrants
  azimuth = comp_azimuth(u, q, radial_azimuth=radial_azimuth)

  i[zero] = 0.0
  azimuth[zero] = 0.0
  radial_azimuth[zero] = -999.0

  ; compute linear polarization
  l = sqrt(q^2 + u^2)

  ; compute doppler shift and linewidth from analytic gaussian fit
  i1 = comp_obs[*, *, 0L, wave_indices[0]]
  i2 = comp_obs[*, *, 0L, wave_indices[1]]
  i3 = comp_obs[*, *, 0L, wave_indices[2]]
  d_lambda = abs(wave[wave_indices[1]] - wave[wave_indices[0]])

  comp_analytic_gauss_fit2, i1, i2, i3, d_lambda, dop, width, i_cent
  dop += rest

  ; TODO: should this be divided by sqrt(2.0) to give sigma?
  width *= c / wave[wave_indices[1]]

  pre_corr = dblarr(nx, ny, 2)
  pre_corr[*, *, 0] = i_cent
  pre_corr[*, *, 1] = dop

  comp_doppler_correction, pre_corr, post_corr, wave_type, ewtrend, temptrend
  if (abs(temptrend) gt 0.01) then begin
    mg_log, 'potential bad doppler correction: temptrend = %f', temptrend, $
            name='comp', /warn
  endif
  corrected_dop = reform(post_corr[*, *, 1])
  corrected_dop[zero] = !values.f_nan

  good_pixel_mask = (i2 gt int_min_thresh) and (i2 lt int_max_thresh) $
                      and (i1 gt 0.01) and (i3 gt 0.01)
  good_pixels = where(good_pixel_mask, complement=bad_pixels, ncomplement=n_bad_pixels)
  if (n_bad_pixels gt 0L) then corrected_dop[bad_pixels] = !values.f_nan

  ; difference between calculated peak intensity and measured is not too great
  ind = where(abs(i_cent - i2) gt 1.5 * i2, count)
  if (count gt 0L) then corrected_dop[ind] = !values.f_nan

  ; write fit parameters to output file

  quick_invert_filename = string(date_dir, wave_type, _method, type, $
                                 format='(%"%s.comp.%s.quick_invert.%s.%s.fts")')
  fits_open, quick_invert_filename, fcbout, /write

  ; copy the primary header from the median file to the output file
  fits_write, fcbout, 0, primary_header

  sxdelpar, header, 'POLSTATE'
  sxdelpar, header, 'WAVELENG'
  sxdelpar, header, 'DATATYPE'
  sxdelpar, header, 'FILTER'
  sxdelpar, header, 'COMMENT'

  sxaddpar, header, 'NTUNES', ntune
  sxaddpar, header, 'LEVEL   ', 'L2'
  sxaddpar, header, 'DATAMIN', min(i, /nan), ' MINIMUM DATA VALUE'
  sxaddpar, header, 'DATAMAX', max(i, /nan), ' MAXIMUM DATA VALUE'
  fits_write, fcbout, i, header, extname='I'

  sxaddpar, header, 'DATAMIN', min(q, /nan), ' MINIMUM DATA VALUE'
  sxaddpar, header, 'DATAMAX', max(q, /nan), ' MAXIMUM DATA VALUE'
  fits_write, fcbout, q, header, extname='Q'

  sxaddpar, header, 'DATAMIN', min(u, /nan), ' MINIMUM DATA VALUE'
  sxaddpar, header, 'DATAMAX', max(u, /nan), ' MAXIMUM DATA VALUE'
  fits_write, fcbout, u, header, extname='U'

  sxdelpar, header, 'COMMENT'
  sxaddpar, header, 'DATAMIN', min(l, /nan), ' MINIMUM DATA VALUE'
  sxaddpar, header, 'DATAMAX', max(l, /nan), ' MAXIMUM DATA VALUE'
  fits_write, fcbout, l, header, extname='Linear Polarization'

  sxaddpar, header, 'COMMENT', $
            'Azimuth is measured positive counter-clockwise from the horizontal.'
  sxaddpar, header, 'DATAMIN', min(azimuth, /nan), ' MINIMUM DATA VALUE'
  sxaddpar, header, 'DATAMAX', max(azimuth, /nan), ' MAXIMUM DATA VALUE'
  fits_write, fcbout, azimuth, header, extname='Azimuth'
  sxdelpar, header, 'COMMENT'

  sxaddpar, header, 'DATAMIN', min(corrected_dop, /nan), ' MINIMUM DATA VALUE'
  sxaddpar, header, 'DATAMAX', max(corrected_dop, /nan), ' MAXIMUM DATA VALUE'
  fits_write, fcbout, corrected_dop, header, extname='Doppler Velocity'

  sxaddpar, header, 'DATAMIN', min(width, /nan), ' MINIMUM DATA VALUE'
  sxaddpar, header, 'DATAMAX', max(width, /nan), ' MAXIMUM DATA VALUE'
  fits_write, fcbout, width, header, extname='Line Width'

  sxaddpar, header, 'DATAMIN', min(radial_azimuth, /nan), ' MINIMUM DATA VALUE'
  sxaddpar, header, 'DATAMAX', max(radial_azimuth, /nan), ' MAXIMUM DATA VALUE'
  fits_write, fcbout, radial_azimuth, header, extname='Radial Azimuth'

  fits_close, fcbout

  zip_cmd = string(quick_invert_filename, format='(%"gzip -f %s")')
  spawn, zip_cmd, result, error_result, exit_status=status
  if (status ne 0L) then begin
    mg_log, 'problem zipping quick_invert file with command: %s', zip_cmd, $
            name='comp', /error
    mg_log, '%s', error_result, name='comp', /error
  endif

  done:
  mg_log, 'done', name='comp', /info
end
