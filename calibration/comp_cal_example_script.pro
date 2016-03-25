devicelib
imagelib

beam = -1
wave = 1075
exact_waves = [1074.38, 1074.50, 1074.62, 1074.74, 1074.86]
locations = [[483, 542], [109, 902]] ; put location in upper left for beam -1
initial_retardances = [83.0, 90.0, 95.0]

;cal_directory = '/hao/solar4/plowman/CoMP/raw/20150729/'
cal_directory = '/export/data1/Data/CoMP/raw.calibration/20150729/'
plot_dir = '/export/data1/Data/CoMP/calibration_plots2_wtrans/'
coef_plot_dir = '/export/data1/Data/CoMP/calibration_coef_plots2_wtrans/'
config_filename = 'config/comp.mgalloy.compdata.calibration.cfg'

if (~file_test(cal_directory, /directory)) then begin
  message, 'cal directory not found: ' + cal_directory
endif

if (~file_test(config_filename)) then begin
  message, 'configure file not found: ' + config_filename
endif

for r = 0L, n_elements(initial_retardances) - 1L do begin
  for l = 0L, n_elements(locations) / 2 - 1L  do begin
    location = locations[*, l]
    for e = 0L, n_elements(exact_waves) - 1L do begin
      exact_wave = exact_waves[e]

      ; initialize the paths and common blocks for CoMP pipeline routines                        
      date_dir = file_basename(cal_directory)
      comp_configuration, config_filename=config_filename
      comp_initialize, date_dir
      comp_setup_loggers
      comp_setup_loggers_date, date_dir

      @comp_config_common

      basename = string(location, exact_wave, initial_retardances[r], $
                        format='(%"cal-section5x5-x%d-y%d-%0.2f-ret%d")')
      mg_log, name='cal', logger=logger
      logger->setProperty, format='%(time)s %(levelshortname)s: %(message)s', $
                           level=5, $
                           filename=filepath(basename + '.log', $
                                             subdir=[date_dir, 'newchi2-results'], $
                                             root=process_basedir)

      ; initialize as well as apply flats/darks
      if (n_elements(reload) eq 0 or keyword_set(reload)) then begin
        comp_init_powfunc_comblk, cal_directory, wave, beam, date_dir, exact_wave, location
      endif

      ;reload=0

      common comp_cal_comblk, xybasis, xyb_upper, xyb_lower, dataupper, datalower, $
                              varsupper, varslower, xmat, ymat, cpols, pangs, crets, $
                              upols, datapols, datacals, cal_data, uppercoefs, $
                              lowercoefs, uppermask, lowermask, data, vars, mask, $
                              nstokes, ucals, calvars, calvar_solve

      calvar_labels = ['I in', 'Q in', 'U in', 'V in', 'Pol trans', 'P ang err', $
                       'Ret trans', 'Retardance', 'Ret ang']

      ; This vector holds the calibration optics variables:
      calvars = dblarr(9)
      calvars[0:3] = [1., 0., 0., 0.] ; The input Stokes vector.
      calvars[4] = 0.45               ; Calibration polarizer transmission.
      calvars[5] = 0.0                ; Systematic offset error in the polarizer angle (in degrees).
      calvars[6] = 0.99               ; Calibration retarder transmission.
      calvars[7] = initial_retardances[r] ; Calibration retarder retardance (in degrees).
      calvars[8] = 0.0                ; Calibration retarder angle (in degrees).

      ; Scales for initial guesses:
      scales = dblarr(9)
      scales[0:3] = [0.05, 0.05, 0.05, 0.05] ; The input Stokes vector.
      scales[4] = 0.05                       ; Calibration polarizer transmission.
      scales[5] = 5.0   ; Systematic offset error in the polarizer angle (in degrees).
      scales[6] = 0.01  ; Calibration retarder transmission.
      scales[7] = 5.0   ; Calibration retarder retardance (in degrees).
      scales[8] = 5.0   ; Calibration retarder angle (in degrees).

      ; Flags for which calibration variables the amoeba should search for:
      solve_flags = intarr(9)
      solve_flags[0:3] = [0, 1, 1, 0] ; The input Stokes vector.
      solve_flags[4] = 1              ; Calibration polarizer transmission.
      solve_flags[5] = 0              ; Systematic offset error in the polarizer angle.
      solve_flags[6] = 1              ; Calibration retarder transmission.
      solve_flags[7] = 1              ; Calibration retarder retardance.
      solve_flags[8] = 1              ; Calibration retarder angle.

      calvar_solve = where(solve_flags)
      guess = calvars[calvar_solve]
      scale = scales[calvar_solve]
      params = amoeba(1.0e-5, function_name='comp_cal_powfunc', p0=guess, scale=scale, ncalls=ncalls)
      chi2 = comp_cal_powfunc(params, diag_plot_dir=plot_dir, $
                              model_basename=basename)
      mg_log, 'Wavelength = %0.2f', exact_wave, name='cal', /info
      mg_log, 'Number of function evaluations in AMOEBA: %d', ncalls, name='cal', /info
      ;mg_log, 'Params: %s', strjoin(strtrim(params, 2), ', '), name='cal', /info
      mg_log, 'Final chi^2: %f', chi2, name='cal', /info

      for i = 0, 8 do mg_log, '%s = %f', calvar_labels[i], calvars[i], name='cal', /info

      dims = size(uppercoefs, /dimensions)
      n_uniq_pols = n_elements(upols)
      n_basis = n_elements(xybasis[0, 0, *])
      stokeslabels = ['I', 'Q', 'U', 'V']

      for c = 0L, n_uniq_pols - 1L do begin
        for s = 0L, nstokes - 1L do begin
          mg_log, 'uppercoefs[%s, %s] = %f', stokeslabels[s], upols[c], uppercoefs[c, s], $
                  name='cal', /info
        endfor
      endfor

      for c = 0L, n_uniq_pols - 1L do begin
        for s = 0L, nstokes - 1L do begin
          mg_log, 'lowercoefs[%s, %s] = %f', stokeslabels[s], upols[c], lowercoefs[c, s], $
                  name='cal', /info
        endfor
      endfor

      ; This structure holds the essential calibration information:
      cal_struct = {xybasis:xybasis, $
                    xmat:xmat, $
                    ymat:ymat, $
                    cpols:cpols, $
                    pangs:pangs, $
                    crets:crets, $
                    upols:upols, $
                    datapols:datapols, $
                    datacals:datacals, $
                    uppercoefs:uppercoefs, $
                    lowercoefs:lowercoefs, $
                    uppermask:uppermask, $
                    lowermask:lowermask, $
                    mask:mask, $
                    ucals:ucals, $
                    calvars:calvars, $
                    calvar_solve:calvar_solve, $
                    calvar_labels:calvar_labels, $
                    chi2:chi2}

      save, cal_struct, params, $
            filename=filepath('calibration_structure_wtrans.sav', $
                        subdir=date_dir, $
                        root=process_basedir)

      comp_make_coef_plots, coef_plot_dir
    endfor
  endfor
endfor

end
