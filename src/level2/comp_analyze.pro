; docformat = 'rst'

;+
; Procedure to fit comp observations with model profiles
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;   
; :Keywords:
;   synthetic : in, optional, type=boolean
;     set to process synthetic data
;   error : out, optional, type=long
;     set to a named variable to return the error status of the routine, 0 for
;     success, anything else for failure 
;
; :Author:
;   MLSO Software Team
;-
pro comp_analyze, date_dir, wave_type, synthetic=synthetic, error=error
  compile_opt strictarr
  @comp_simulate_common
  @comp_config_common
  @comp_constants_common

  mg_log, 'wave_type: %s', wave_type, name='comp', /info

  ; establish error handler for a crash in this routine
  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    mg_log, /last_error, name='comp'
    return
  endif

  l2_process_dir = filepath('level2', subdir=date_dir, root=process_basedir)
  cd, l2_process_dir

  ; find line center
  case wave_type of
    '1074' : line_center = center1074
    '1079' : line_center = center1079
    '1083' : line_center = center1083
  endcase

  wavelength = line_center   ; rest wavelength of emission line

  ;  read in comp observations
  if (keyword_set(process_synthetic)) then begin
    file = string(date_dir, wave_type, format='(%"%s.comp.%s.synthetic.fts.gz")')
  endif else begin
    method = 'mean'
    file = string(date_dir, wave_type, method, format='(%"%s.comp.%s.%s.fts.gz")')
  endelse

  if (~file_test(file) || file_test(file, /zero_length)) then begin
    mg_log, '%s does not exist, exiting', file, name='comp', /warn
    return
  endif

  fits_open, file, fcb
  n = fcb.nextend

  ; determine tunings of filter

  waves = fltarr(n - 1)
  pol   = strarr(n - 1)
  for i = 0, n - 2 do begin
    fits_read, fcb, dat, header, /header_only, exten_no=i + 1, /no_abort, message=msg
    if (msg ne '') then message, msg
    waves[i] = sxpar(header, 'WAVELENG')
    pol[i]   = sxpar(header, 'POLSTATE')
  endfor

  tunings = waves[uniq(waves, sort(waves))]
  ntune   = n_elements(tunings)
  stokes  = pol[uniq(pol, sort(pol))]
  nstokes = n_elements(stokes)

  ;  read data

  comp_obs = fltarr(nx, ny, 4, ntune)
  i = 1
  for is = 0, nstokes - 1 do begin
    for iw = 0, ntune - 1 do begin
      fits_read, fcb, dat, header, exten_no=i, /no_abort, message=msg
      if (msg ne '') then message, msg
      comp_obs[*, *, is, iw] = dat
      i += 1
    endfor
  endfor

  ; copy the primary header from the mean file to the output file
  fits_read, fcb, d, primary_header, /header_only, exten_no=0, /no_abort, message=msg
  fits_close, fcb
  if (msg ne '') then begin
    message, msg
  endif

  ;  create wavelength scale (nm)

  range = 1.25   ; wavelength range around rest wavelength (nm)
  num = 251      ; number of points in profiles (make odd)
  ;num=101        ; number of points in profiles (make odd)

  lambda = dindgen(num) / float(num - 1) * range + wavelength - range / 2.0

  ; compute simulated filter transmission profiles
  ; and normalize them to unit area

  filters = fltarr(num, ntune)

  for i = 0, ntune - 1 do filters[*, i] = comp_sinc(lambda, tunings[i])
  norm = total(filters, 1)
  for i = 0, ntune - 1 do filters[*, i] = filters[*, i] / norm[i]

  if (debug eq 1) then begin
    window, 0, xsize=800, ysize=600
    plot, [min(lambda), max(lambda)], [0.0, 1.0], /nodata, xstyle=1
    for i = 0, ntune - 1 do oplot, lambda, filters[*, i] / max(filters)
    ans = ' '
    read, 'enter any character', ans
  endif

  ;  loop to fit observations

  ;  the parameters of the fit are:
  ;  0 - intensity
  ;  1 - velocity
  ;  2 - line_width
  ;  3 - bfield
  ;  4 - linear
  ;  5 - azimuth
  ;  6 - offset in Stokes Q
  ;  7 - offset in Stokes U
  ;  8 - offset in Stokes V
  ;  9 - Stokes U-V crosstalk
  
  nparam = 10
  fit_intensity  = fltarr(nx,ny)
  fit_velocity   = fltarr(nx,ny)
  fit_line_width = fltarr(nx,ny)
  fit_bfield     = fltarr(nx,ny)
  fit_linear     = fltarr(nx,ny)
  fit_azimuth    = fltarr(nx,ny)
  q_offset       = fltarr(nx,ny)
  u_offset       = fltarr(nx,ny)
  v_offset       = fltarr(nx,ny)
  crosstalk      = fltarr(nx,ny)
  iterations     = intarr(nx,ny)
  max_intensity  = fltarr(nx,ny)
  fit_guesses    = fltarr(nx,ny,nparam)
  
  guess          = fltarr(nparam, /nozero)
  obs            = fltarr(4, ntune)
  center_index   = ntune / 2
  
  mg_log, 'loop over pixels', name='comp', /debug
  for ix = 0, nx - 1 do begin
    for iy = 0, ny - 1 do begin
      ; use Powell's method, DPFMIN, or MPFIT to fit profile parameters

      if (debug eq 1) then begin
        ; select a specific point to troubleshoot
        window, 0, xsize=600, ysize=600
        tvscl, comp_obs[*, *, 0, 2]
        cursor, ix, iy, /device
      endif

      obs[*, *] = comp_obs[ix, iy, *, *]
      maxi = max(obs)
      
      if (debug eq 1) then begin
        ; plot the lines
        window, 0, xsize=800, ysize=800
        !p.multi = [0, 2, 2, 0, 0]
        plot, tunings, obs[0, *], psym=1, title='Stokes I'
        plot, tunings, obs[1, *], psym=1, yrange=[-.5,.5], title='Stokes Q'
        plot, tunings, obs[2, *], psym=1, yrange=[-.5,.5], title='Stokes U'
        plot, tunings, obs[3, *], psym=1, yrange=[-.1,.1], title='Stokes V'
        ans = ' '
        read, 'enter any character', ans
      endif
      
      ;  only fit if intensity is above a threshhold
;      if process_synthetic eq 1 or (mask(ix,iy) eq 1 and maxi gt 1.) then begin
      if (keyword_set(synthetic) $
            or comp_obs[ix, iy, 0, center_index] gt 0.0 $
            and maxi gt 1.0) then begin
        guess[0] = maxi * 1.2
        case ntune of
          3 : begin
            guess[1] = -20.0 * (obs[0, 0] - obs[0, 2]) / (obs[0, 0] + obs[0, 2]) + 8.0
            guess[3] = -12000.e0*(obs[3, 0] - obs[3, 2]) / (obs[0, 0] + obs[0, 2])
          end
          5 : begin
            guess[1] = -22.0 * (obs[0, 1] - obs[0, 3]) / (obs[0, 1] + obs[0, 3])
            guess[3] = -12000.e0 * (obs[3, 1] - obs[3, 3]) / (obs[0, 1] + obs[0, 3])
          end
          7 : begin
            guess[1] = -22.0 * (obs[0, 2] - obs[0, 4]) / (obs[0, 2] + obs[0, 4])
            guess[3] = -12000.e0 * (obs[3, 2] - obs[3, 4]) / (obs[0, 2] + obs[0, 4])
          end
        endcase
        guess[2] = 30.0
        guess[4] = sqrt(total(obs[1, *])^2 + total(obs[2, *])^2) / total(obs[0, *])
        guess[5] = atan(total(obs[2, *]), total(obs[1, *])) * 90.0 / !pi
        guess[6] = 0.0
        guess[7] = 0.0
        guess[8] = 0.0
        guess[9] = 0.0
        
        ; a = guess
        fit_guesses[ix, iy, *] = guess
        
        ; set parinfo.mpside=3 for analytical derivatives.
        parinfo = replicate({value:0.0, limited:[0, 0], limits:[0.0, 0.0], mpside:0}, 10)
        parinfo.value = guess
        ; parinfo[0].limited = [1,1] & parinfo[0].limits = [0.,50.] ; intensity
        ; parinfo[1].limited = [1,1] & parinfo[1].limits = [-20.,20.] ; velocity
        ; parinfo[2].limited = [1,1] & parinfo[2].limits = [10.,75.] ; line width
        ; parinfo[3].limited = [1,1] & parinfo[3].limits = [-100.,100.] ; B
        ; parinfo[4].limited = [1,1] & parinfo[4].limits = [0.,.3] ; polarization
        ; parinfo[5].limited = [1,1] & parinfo[5].limits = [-90.,90.] ; azimuth
        iter = 201
        a = mpfit('comp_fit_iquv', parinfo=parinfo, niter=iter, xtol=1e-4, /nocatch, /quiet)
        ; a = mpfit('fit_iquv', parinfo=parinfo, niter=iter, xtol=1e-4, ERRMSG=errmsg)
        
        if (debug eq 1) then begin
          !p.multi = [0, 2, 2, 0, 0]
          print, ix, iy, iter
          print, guess
          print, a
          
          oplot, tunings, obs_fit[0, *]
          oplot, tunings, obs_fit[1, *]
          oplot, tunings, obs_fit[2, *]
          oplot, tunings, obs_fit[3, *]
          
          ans = ' '
          read, 'enter any character', ans
        endif
        
        if (debug eq 2) then begin
          !p.multi = [0, 2, 2, 0, 0]
          print, ix, iy, iter
          print, guess
          print, a
          print, xi[0, 0], xi[1, 1], xi[2, 2], xi[3, 3], xi[4, 4], xi[5, 5]
          
          window, 0, xsize=800, ysize=800, xpos=300
          plot, tunings, obs[0, *], psym=1, title='Stokes I'
          oplot, tunings, obs_fit[0, *]
          plot, tunings, obs[1, *], psym=1 ,yrange=[-0.5, 0.5], title='Stokes Q'
          oplot, tunings, obs_fit[1, *]
          plot, tunings, obs[2, *], psym=1, yrange=[-0.5, 0.5], title='Stokes U'
          oplot, tunings, obs_fit[2, *]
          plot, tunings, obs[3, *], psym=1, yrange=[-0.1, 0.1], title='Stokes V'
          oplot, tunings, obs_fit[3, *]
          
          empty
          wait, 1.02
        endif
        
        if (iter lt 200) then begin
          fit_intensity[ix, iy]  = a[0]
          fit_velocity[ix, iy]   = a[1]
          fit_line_width[ix, iy] = a[2]
          fit_bfield[ix, iy]     = a[3]
          fit_linear[ix, iy]     = a[4]
          fit_azimuth[ix, iy]    = a[5]
          q_offset[ix, iy]       = a[6]
          u_offset[ix, iy]       = a[7]
          v_offset[ix, iy]       = a[8]
          crosstalk[ix, iy]      = a[9]
        endif else begin
          mg_log, 'exceeded 200 iterations', name='comp', /warn
        endelse
        iterations[ix, iy] = iter
      endif
      max_intensity[ix, iy] = maxi
    endfor
  endfor
  
  ; write iterations to FITS file
  fits_write, date_dir + '.comp.' + wave_type + '.iterations.fts', iterations
  fits_write, date_dir + '.comp.' + wave_type + '.maxi.fts', max_intensity

  ; write fit parameters to output file
  
  mg_log, 'write output files', name='comp', /info

  save, file=date_dir + '.comp.' + wave_type + '.invert.sav', $
        fit_intensity, fit_velocity, fit_line_width, $
        fit_bfield, fit_linear, fit_azimuth, q_offset, u_offset, v_offset, $
        crosstalk
    
  fits_open, date_dir + '.comp.' + wave_type + '.invert.fts', fcbout, /write

  ; copy the primary header from the mean file to the output file
  fits_write, fcbout, 0, primary_header

  sxdelpar, header, 'POLSTATE'
  sxdelpar, header, 'WAVELENG'
  sxdelpar, header, 'DATATYPE'
  sxdelpar, header, 'FILTER'
  sxdelpar, header, 'COMMENT'
  sxaddpar, header, 'NTUNE', ntune
  for i = 0, ntune - 1 do begin
    sxaddpar, header, string(format='("WAVE(",i1,")")', i), tunings[i]
  endfor
  sxaddpar, header, 'LEVEL   ', 'L2'
  
  ; write inversion results
  fits_write, fcbout, fit_intensity, header, extname='Intensity'
  fits_write, fcbout, fit_velocity, header, extname='Velocity'
  fits_write, fcbout, fit_line_width, header, extname='Line Width'
  fits_write, fcbout, fit_bfield, header, extname='B Field'
  fits_write, fcbout, fit_linear, header, extname='Linear Polarization'
  fits_write, fcbout, fit_azimuth, header, extname='Azimuth'
  fits_write, fcbout, q_offset, header, extname='Q Offset'
  fits_write, fcbout, u_offset, header, extname='U Offset'
  fits_write, fcbout, v_offset, header, extname='V Offset'
  fits_write, fcbout, crosstalk, header, extname='Crosstalk'
  fits_close, fcbout
 
  ; compress files
  mg_log, 'compressing files', name='comp', /debug
  zip_cmd = 'gzip -f ' + date_dir + '.comp.' + wave_type + '.invert.*.fts'
  spawn, zip_cmd, result, error_result, exit_status=status
  if (status ne 0L) then begin
    mg_log, 'problem zipping quick_invert file(s) with command: %s', zip_cmd, $
            name='comp', /error
    mg_log, '%s', error_result, name='comp', /error
  endif

  ; write starting guesses
  fits_open, date_dir + '.comp.' + wave_type + '.guesses.fts', fcbout, /write
  fits_write, fcbout, fit_guesses[*,*,0], header, extname='Intensity'
  fits_write, fcbout, fit_guesses[*,*,1], header, extname='Velocity'
  fits_write, fcbout, fit_guesses[*,*,2], header, extname='Line Width'
  fits_write, fcbout, fit_guesses[*,*,3], header, extname='B Field'
  fits_write, fcbout, fit_guesses[*,*,4], header, extname='Linear Polarization'
  fits_write, fcbout, fit_guesses[*,*,5], header, extname='Azimuth'
  fits_write, fcbout, fit_guesses[*,*,6], header, extname='Q Offset'
  fits_write, fcbout, fit_guesses[*,*,7], header, extname='U Offset'
  fits_write, fcbout, fit_guesses[*,*,8], header, extname='V Offset'
  fits_write, fcbout, fit_guesses[*,*,9], header, extname='Crosstalk'
  fits_close, fcbout

  done:
  mg_log, 'done', name='comp', /info
end

; main-level example

config_filename = filepath('comp.mgalloy.mahi.latest.cfg', $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())
comp_configuration, config_filename=config_filename
comp_initialize, '20110523'

;comp_analyze, '20110504', '1074', /synthetic
comp_analyze, '20110523', '1074'

end
