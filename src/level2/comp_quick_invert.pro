; docformat = 'rst'

;+
; Procedure to perform an approximate 'quick' inversion of some parameters from
; the comp averaged Level_2 data file. This routine reads the
; YYYYMMDD.comp.wwww.mean.fts which was computed with `comp_average`, where
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
;   comp_analytic_gauss_fit2, fits_open, fits_read, fits_write, fits_close,
;   sxpar, sxaddpar, sxdelpar, mg_log
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
;
; :Author:
;   Sitongia, Tomczyk
;
; :History:
;   removed gzip    Oct 1 2014  GdT
;   removed copy_file of intensity fits to archive_dir  Oct 1 2014  GdT
;-
pro comp_quick_invert, date_dir, wave_type, synthetic=synthetic, error=error
  compile_opt idl2
  @comp_simulate_common
  @comp_constants_common
  @comp_config_common

  mg_log, 'quick invert %s', wave_type, name='comp', /info

  ; Establish error handler. When errors occur, the index of the
  ; error is returned in the variable Error_status:
  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    mg_log, /last_error, name='comp'
    return
  endif

  l1_process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)
  l2_process_dir = filepath('', subdir=[date_dir, 'level2'], root=process_basedir)
  cd, l2_process_dir

  if (keyword_set(synthetic)) then begin
    process_synthetic = 1
  endif else begin
    process_synthetic = 0
  endelse

  ; create filename and open input FITS file
  if (process_synthetic eq 1) then begin
    file = string(date_dir, wave_type, format='(%"%s.comp.%s.synthetic.fts")')
  endif else begin
    file = string(date_dir, wave_type, format='(%"%s.comp.%s.mean.fts")')
  endelse

  if (~file_test(file) || file_test(file, /zero_length)) then begin
    mg_log, 'file %s does not exist, exiting', file, name='comp/quick_invert', /info
    return
  endif

  fits_open, file, fcb
  n = fcb.nextend

  ; copy the primary header from the mean file to the output file
  fits_read, fcb, d, primary_header, /header_only, exten_no=0

  ntune = sxpar(primary_header, 'NTUNE', count=nrecords)
  if (nrecords eq 0L) then ntune = sxpar(primary_header, 'NTUNES')
  nstokes = 4
  center_index = ntune / 2

  ; read data

  comp_obs = fltarr(nx, nx, nstokes, ntune)
  wave = fltarr(ntune)

  i = 1
  for is = 0L, nstokes - 1L do begin
    for iw = 0L, ntune - 1L do begin
      fits_read, fcb, dat, header, exten_no=i
      comp_obs[*, *, is, iw] = dat
      wave[iw] = sxpar(header, 'WAVELENG')
      ++i
    endfor
  endfor

  fits_close, fcb

  sxaddpar, primary_header, 'N_EXT', 7, /savecomment
  sxaddpar, primary_header, 'VERSION', code_revision, ' Software Subversion Revision'

  ; compute parameters
  i = comp_obs[*, *, 0, center_index]
  q = comp_obs[*, *, 1, center_index]
  u = comp_obs[*, *, 2, center_index]

  p_angle = sxpar(header,'SOLAR_P0')

  zero = where(i eq 0, count)
  if (count eq 0) then mg_log, 'no zeros', name='comp/quick_invert', /warn

  ; compute azimuth and adjust for p-angle, correct azimuth for quadrants  
  azimuth = 0.5 * atan(u, q) * 180. / !pi - p_angle + 45.
  ;azimuth = -90. > azimuth < 90.
  azimuth = azimuth mod 180.
  bad = where(azimuth lt 0., count)
  if (count gt 0L) then azimuth[bad] = azimuth[bad] + 180.

  i[zero] = 0.
  azimuth[zero] = 0.

  ; compute linear polarization
  l = sqrt(q^2 + u^2)

  ; compute doppler shift and linewidth from analytic gaussian fit
  i1 = comp_obs[*, *, 0L, center_index - 1L]
  i2 = comp_obs[*, *, 0L, center_index]
  i3 = comp_obs[*, *, 0L, center_index + 1L]
  d_lambda = abs(wave[center_index] - wave[center_index - 1L])

  comp_analytic_gauss_fit2, i1, i2, i3, d_lambda, dop, width, i_cent
  dop *= 3.e5 / wave[center_index]   ; convert to km/s
  width *= 3.e5 / wave[center_index]

  ; write fit parameters to output file

  fits_open, string(date_dir, wave_type, $
                    format='(%"%s.comp.%s.quick_invert.fts")'), $
             fcbout, /write

  ; copy the primary header from the mean file to the output file
  fits_write, fcbout, 0, primary_header

  sxdelpar, header, 'POLSTATE'
  sxdelpar, header, 'WAVELENG'
  sxdelpar, header, 'DATATYPE'
  sxdelpar, header, 'FILTER'
  sxdelpar, header, 'COMMENT'
  sxaddpar, header, 'NTUNES', ntune
  sxaddpar, header, 'LEVEL   ', 'L2'
  sxaddpar, header, 'DATAMIN', min(i), ' MINIMUM DATA VALUE'
  sxaddpar, header, 'DATAMAX', max(i), ' MAXIMUM DATA VALUE'

  fits_write, fcbout, i, header, extname='I'

  sxaddpar, header, 'DATAMIN', min(q), ' MINIMUM DATA VALUE'
  sxaddpar, header, 'DATAMAX', max(q), ' MAXIMUM DATA VALUE'
  fits_write, fcbout, q, header, extname='Q'

  sxaddpar, header, 'DATAMIN', min(u), ' MINIMUM DATA VALUE'
  sxaddpar, header, 'DATAMAX', max(u), ' MAXIMUM DATA VALUE'
  fits_write, fcbout, u, header, extname='U'

  sxdelpar, header, 'COMMENT'
  sxaddpar, header, 'DATAMIN', min(l), ' MINIMUM DATA VALUE'
  sxaddpar, header, 'DATAMAX', max(l), ' MAXIMUM DATA VALUE'

  fits_write, fcbout, l, header, extname='Linear Polarization'

  sxaddpar, header, 'COMMENT', $
            'Azimuth is measured positive counter-clockwise from the horizontal.'
  sxaddpar, header, 'DATAMIN', min(azimuth), ' MINIMUM DATA VALUE'
  sxaddpar, header, 'DATAMAX', max(azimuth), ' MAXIMUM DATA VALUE'

  fits_write, fcbout, azimuth, header, extname='Azimuth'
  sxdelpar, header, 'COMMENT'

  fits_write, fcbout, dop, header, extname='Doppler Velocity'
  sxaddpar, header, 'DATAMIN', min(dop), ' MINIMUM DATA VALUE'
  sxaddpar, header, 'DATAMAX', max(dop), ' MAXIMUM DATA VALUE'

  fits_write, fcbout, width, header, extname='Line Width'
  sxaddpar, header, 'DATAMIN', min(width), ' MINIMUM DATA VALUE'
  sxaddpar, header, 'DATAMAX', max(width), ' MAXIMUM DATA VALUE'

  fits_close, fcbout

  ; for the directory name
  year = strmid(date_dir, 0, 4)
  month = strmid(date_dir, 4, 2)
  day = strmid(date_dir, 6, 4)
  destination = filepath('', subdir=[year, month, day], root='.')

  ; copy FITS files to
  ;file_copy, date_dir+'.comp.'+wave_type+'.quick_invert.fts', archive_dir + destination, /OVERWRITE

  mg_log, 'done', name='comp', /info
end
