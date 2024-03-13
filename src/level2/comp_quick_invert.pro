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
;   Q - approximated by the Stokes Q image nearest line center
;   U - approximated by the Stokes U image nearest line center
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
    mg_log, '%s not found', file, name='comp', /warn
    return
  endif

  fits_open, file, fcb
  n = fcb.nextend

  comp_inventory, fcb, beam, wavelengths, error=error
  if (error gt 0L) then begin
    mg_log, 'error reading %s', file, name='comp', /error
    goto, done
  endif

  ; copy the primary header from the median file to the output file
  fits_read, fcb, d, primary_header, /header_only, exten_no=0, $
             /no_abort, message=msg
  if (msg ne '') then begin
    fits_close, fcb
    mg_log, 'problem reading %s', file, name='comp', /error
    message, msg
  endif

  sxdelpar, primary_header, 'OBS_PLAN'
  sxdelpar, primary_header, 'OBS_ID'

  ntune = sxpar(primary_header, 'NTUNE', count=nrecords)
  if (nrecords eq 0L) then ntune = sxpar(primary_header, 'NTUNES')

  nstokes = n / ntune - 1L   ; don't count BKG

  if (nstokes lt 3L) then begin
    mg_log, 'only %d Stokes parameter%s in average file', $
            nstokes, nstokes ne 1 ? 's' : '', $
            name='comp', /warn
    mg_log, 'quitting', name='comp', /warn
    goto, done
  endif

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
      fits_read, fcb, dat, h, exten_no=e, /no_abort, message=msg
      if (msg ne '') then message, msg
      comp_obs[*, *, is, iw] = dat
      wave[iw] = sxpar(h, 'WAVELENG')
      ++e
    endfor
  endfor

  ; use header for center wavelength for I as template
  fits_read, fcb, dat, header, exten_no=ntune / 2, /no_abort, message=msg
  if (msg ne '') then message, msg

  fits_close, fcb

  sxaddpar, primary_header, 'N_EXT', 8, /savecomment

  mask = comp_l2_mask(primary_header)
  no_post_mask = comp_l2_mask(primary_header, /no_post)

  case wave_type of
    '1074': begin
        rest = double(center1074)
        nominal = double(nominal_1074)
        int_min_thresh = int_min_1074_thresh
      end
    '1079': begin
        rest = double(center1079)
        nominal = double(nominal_1079)
        int_min_thresh = int_min_1079_thresh
      end
    '1083': begin
        rest = double(center1083)
        nominal = double(center1083)
        int_min_thresh = int_min_1079_thresh
      end
  endcase
  c = 299792.458D

  sxaddpar, primary_header, 'METHOD', _method, $
            ' Input file type used for quick invert'

  ; update version
  comp_l2_update_version, primary_header

  i1 = comp_obs[*, *, 0, wave_indices[0]] 
  i2 = comp_obs[*, *, 0, wave_indices[1]]       ; center line intensity
  i3 = comp_obs[*, *, 0, wave_indices[2]] 
  q = comp_obs[*, *, 1, wave_indices[1]]        ;center line q
  u = comp_obs[*, *, 2, wave_indices[1]]        ;center line u 

  zero = where(i2 le 0, count)
  if (count eq 0) then begin
    mg_log, 'no values less than 0 for %s nm intensity [%s] (%s)', $
            wave_type, _method, type, $
            name='comp', /warn
  endif

  ; compute azimuth and adjust for p-angle, correct azimuth for quadrants
  azimuth = comp_azimuth(u, q, radial_azimuth=radial_azimuth)

  ; compute linear polarization
  l = sqrt(q^2 + u^2)

  ; i1 and i3 not used in quick invert - mask criteria only based on line center
  good_pol_indices = where(mask gt 0 $
                             and i2 gt 0.25 $
                             and i2 lt 60.0, complement=bad_pol_indices, /null)

  ; mask polarization
  i2[bad_pol_indices]             = 0D
  q[bad_pol_indices]              = 0D
  u[bad_pol_indices]              = 0D
  l[bad_pol_indices]              = 0D
  azimuth[bad_pol_indices]        = 0D
  radial_azimuth[bad_pol_indices] = 0D

  ; compute doppler shift and linewidth from analytic gaussian fit
  d_lambda = abs(wave[wave_indices[1]] - wave[wave_indices[0]])

  ; changed the gaussian fit to use the same routine used in l2_analytical to
  ; catch all bad values
  ; comp_analytic_gauss_fit2, i1, i2, i3, d_lambda, dop, width, peak_intensity

  bad_pixels_mask = bytarr(nx, ny)
  temp_data = dblarr(nx, ny, 3)

  for xx = 0L, nx - 1L do begin
    for yy = 0L, ny - 1L do begin
      if (mask[xx, yy] eq 1) then begin
        ; compute analytical gaussfit
        profile = double([reform(i1[xx, yy]), reform(i2[xx, yy]), reform(i3[xx, yy])])
        if min(profile) gt 0 then begin
          comp_analytic_gauss_fit, profile, d_lambda, doppler_shift, width, i_cent
          temp_data[xx, yy, 0] = i_cent
          temp_data[xx, yy, 1] = doppler_shift  
          temp_data[xx, yy, 2] = width 
          ; if gaussian fits cannot be performed update mask of bad pixels
          if doppler_shift eq 0 and width eq 0 and i_cent eq 0 then bad_pixels_mask[xx, yy] = 1B   ; bad_pixels_mask has 1 where gausssian fit could not be done
        endif else begin
          bad_pixels_mask[xx, yy] = 1B    ; bad_pixels_mask has 1 where pixels have negative intensity
          temp_data[xx, yy, 0] = 0.0D
          temp_data[xx, yy, 1] = 0.0D
          temp_data[xx, yy, 2] = 0.0D
         endelse
      endif else begin
        bad_pixels_mask[xx, yy] = 1B    ; bad_pixels_mask has 1 where mask has zero
        temp_data[xx, yy, 0] = 0.0D
        temp_data[xx, yy, 1] = 0.0D
        temp_data[xx, yy, 2] = 0.0D
      endelse
    endfor
  endfor

  ; peak intensity
  peak_intensity = reform(temp_data[*, *, 0])
  peak_intensity[where(bad_pixels_mask eq 1)] = 0D     ; exclude points where gaussian fit could not be performed

  ; convert e-folding line width to FWHM and km/s
  line_width = (reform(temp_data[*, *, 2])) * c / nominal 
  line_width_fwhm = line_width * fwhm_factor
  line_width_fwhm[where(bad_pixels_mask eq 1)] =  0D        ; exclude points where gaussian fit could not be performed

  ; velocity in km/s
  ; "rest" here is the center wavelength
  velo = temp_data[*, *, 1] + rest
  velo *= c / nominal
  velo[where(bad_pixels_mask eq 1)] = 0D         ; exclude points where gaussian fit could not be performed

  ; rest wavelength calculation
  rest_wavelength = comp_compute_rest_wavelength(primary_header, $
                                                 velo, $
                                                 [[[i1]], [[i2]], [[i3]]], $
                                                 line_width_fwhm, $
                                                 method='median', $
                                                 indices=vel_indices, $
                                                 med_east=med_vel_east, $
                                                 med_west=med_vel_west)

  corr_velo = velo

  if (n_elements(vel_indices) gt 0L and finite(rest_wavelength) ne 0) then begin
    ; case in which a rest wavelength was determined 
    corr_velo[vel_indices] = velo[vel_indices] - rest_wavelength
    corr_velo[where(bad_pixels_mask eq 1)] = 0D 

    ; define masking for velocity and line widh data - less restrictive than for movies and images
    good_vel_indices = where(mask eq 1 $
                          and bad_pixels_mask eq 0 $ ; exclude points where gaussian fit could not be performed
                          and abs(corr_velo) lt 100 $
                          and i1 gt 0.1 $
                          and i2 gt int_min_thresh $
                          and i3 gt 0.1 $
                          and i1 lt int_max_thresh $
                          and i2 lt int_max_thresh $
                          and i3 lt int_max_thresh $
                          and line_width_fwhm gt 36 $ 
                          and line_width_fwhm lt 170.0, $
                          ngood, $
                          ncomplement=n_bad_vel_pixels, $
                          complement=bad_vel_indices, $
                         /null)
    ; apply masking to velocity and line width
    if (n_bad_vel_pixels gt 0L) then begin
      velo[bad_vel_indices] =  0D
      corr_velo[bad_vel_indices] =  0D
      line_width_fwhm[bad_vel_indices] = 0D
    endif
  endif else begin
    mg_log, 'rest wavelength not found', name='comp', /warn
    peak_intensity[*] = 0D
    velo[*] = 0D
    corr_velo[*] = 0D
    line_width[*] =  0D
    line_width_fwhm[*] =  0D
  endelse

  ; difference between calculated peak intensity and measured is not too great
  ;ind = where(abs(temp_peak_int - i2) gt 1.5 * i2, count)
  ;if (count gt 0L) then begin
  ;  velo[ind] = !values.f_nan
  ;  corr_velo[ind] = !values.f_nan
  ;endif

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

  sxaddpar, header, 'DATAMIN', min(i2, /nan), ' minimum data value', format='(F0.3)'
  sxaddpar, header, 'DATAMAX', max(i2, /nan), ' maximum data value', format='(F0.3)'
  fits_write, fcbout, i2, header, extname='Center wavelength intensity'

  sxaddpar, header, 'DATAMIN', min(q, /nan), ' minimum data value', format='(F0.3)'
  sxaddpar, header, 'DATAMAX', max(q, /nan), ' maximum data value', format='(F0.3)'
  fits_write, fcbout, q, header, extname='Center wavelength Q'

  sxaddpar, header, 'DATAMIN', min(u, /nan), ' minimum data value', format='(F0.3)'
  sxaddpar, header, 'DATAMAX', max(u, /nan), ' maximum data value', format='(F0.3)'
  fits_write, fcbout, u, header, extname='Center wavelength U'

  sxdelpar, header, 'COMMENT'
  sxaddpar, header, 'DATAMIN', min(l, /nan), ' minimum data value', format='(F0.3)'
  sxaddpar, header, 'DATAMAX', max(l, /nan), ' maximum data value', format='(F0.3)'
  fits_write, fcbout, l, header, extname='Linear polarization'

  sxaddpar, header, 'COMMENT', $
            'Azimuth is measured positive counter-clockwise from the horizontal.'
  sxaddpar, header, 'DATAMIN', min(azimuth, /nan), ' minimum data value', format='(F0.3)'
  sxaddpar, header, 'DATAMAX', max(azimuth, /nan), ' maximum data value', format='(F0.3)'
  fits_write, fcbout, azimuth, header, extname='Azimuth'
  sxdelpar, header, 'COMMENT'

  sxaddpar, header, 'DATAMIN', min(corr_velo, /nan), ' minimum data value', $
            format='(F0.3)'
  sxaddpar, header, 'DATAMAX', max(corr_velo, /nan), ' maximum data value', $
            format='(F0.3)'

  fxaddpar, header, 'RSTWVL', rest_wavelength, $
            ' [km/s] rest wavelength', format='(F0.3)', /null

  fits_write, fcbout, corr_velo, header, extname='Corrected LOS velocity'

  sxdelpar, header, 'RSTWVL'

  sxaddpar, header, 'DATAMIN', min(line_width_fwhm, /nan), ' minimum data value', format='(F0.3)'
  sxaddpar, header, 'DATAMAX', max(line_width_fwhm, /nan), ' maximum data value', format='(F0.3)'
  fits_write, fcbout, line_width_fwhm, header, extname='Line width (FWHM)'

  sxaddpar, header, 'DATAMIN', min(radial_azimuth, /nan), ' minimum data value', $
            format='(F0.3)'
  sxaddpar, header, 'DATAMAX', max(radial_azimuth, /nan), ' maximum data value', $
            format='(F0.3)'
  fits_write, fcbout, radial_azimuth, header, extname='Radial azimuth'

  if (add_uncorrected_velocity) then begin
    sxaddpar, header, 'DATAMIN', min(velo, /nan), ' minimum data value', $
              format='(F0.3)'
    sxaddpar, header, 'DATAMAX', max(velo, /nan), ' maximum data value', $
              format='(F0.3)'
    fxaddpar, header, 'RESTWVL', median_rest_wavelength, ' [km/s] rest wavelength', $
              format='(F0.3)', /null
    fxaddpar, header, 'ERESTWVL', east_median_rest_wavelength, $
              ' [km/s] east rest wavelength', format='(F0.3)', /null
    fxaddpar, header, 'WRESTWVL', west_median_rest_wavelength, $
              ' [km/s] west rest wavelength', format='(F0.3)', /null
    fits_write, fcbout, velo, header, extname='Uncorrected Doppler Velocity'
    sxdelpar, header, 'RESTWVL'
    sxdelpar, header, 'ERESTWVL'
    sxdelpar, header, 'WRESTWVL'
  endif

  sxaddpar, header, 'DATAMIN', min(peak_intensity, /nan), ' minimum data value', format='(F0.3)'
  sxaddpar, header, 'DATAMAX', max(peak_intensity, /nan), ' maximum data value', format='(F0.3)'
  fits_write, fcbout, peak_intensity, header, extname='Peak intensity'

  fits_close, fcbout

  zip_cmd = string(quick_invert_filename, format='(%"gzip -f %s")')
  spawn, zip_cmd, result, error_result, exit_status=status
  if (status ne 0L) then begin
    mg_log, 'problem zipping quick_invert file with command: %s', zip_cmd, $
            name='comp', /error
    mg_log, '%s', error_result, name='comp', /error
  endif

  done:
  fits_close, fcb

  mg_log, 'done', name='comp', /info
end
