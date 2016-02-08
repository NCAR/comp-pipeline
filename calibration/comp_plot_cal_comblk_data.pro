; docformat = 'rst'

;+
; Make plots of the data and fits (if run after fitting is complete) using the
; common block. Plots consist of one file for each calibration optics
; configuration, each showing 8 panels with the nominal I, Q, U, and V (i.e.,
; what you'd get from naive demodulation of the nominal I+Q, I+U, etc ignoring
; crosstalk), data on the left 4 and model fit on the right. File names and
; image titles  have a key identifying the calibration optics configuration,
; which consists of a polarizer state flag (1 = in, 0 = out), retarder state
; flag (1 = in, 0 = out), and polarizer angle, rounded to the nearest degree.
;
; :Params:
;   plot_dir : in, required, type=string
;     the directory to which the plots will be written
;
; :Author:
;   Joseph Plowman
;-
pro comp_plot_cal_comblk_data, plot_dir
  compile_opt strictarr
  common comp_cal_comblk, xybasis, xyb_upper, xyb_lower, dataupper, datalower, $
                          varsupper, varslower, xmat, ymat, cpols, pangs, $
                          crets, upols, datapols, datacals, cal_data, $
                          uppercoefs, lowercoefs, uppermask, lowermask, $
                          data, vars, mask, nstokes, ucals, calvars, $
                          calvar_solve

  ncals = n_elements(ucals)

  ; color table which goes from purple to black to green
  r = [0, reverse(dindgen(127)), dblarr(127), 255]
  b = [0, dblarr(127), dindgen(127), 255]

  ; set up plotting device
  if (n_elements(cal_data) eq n_elements(data)) then begin
    ; 8 panels if cal_data is present
    !p.multi = [0, 4, 2]
    xsize = 16
    ysize = 8
  endif else begin
    ; only plot 4 panels (the data) if cal_data is absent
    !p.multi = [0, 2, 2]
    xsize = 8
    ysize = 8
  endelse
  set_plot, 'ps'
  device, color=1, bits_per_pixel=8, /inches, /encapsulated, $
          xsize=xsize, ysize=ysize, decomposed=0

  ; Loop over unique calibration optics configurations, and make plots for the
  ; nominal I, Q, U, and V for the data and (if present) the calibration fit
  ; to the data. IDL multi-plot ordering goes left to right, then down to the
  ; next row, so we make the plots in the following order: I, Q, fitted I,
  ; fitted Q, U, V, fitted U, fitted V. If there are no calibration fits, the
  ; order is just I, Q, U, V.
  if (~file_test(plot_dir)) then file_mkdir, plot_dir
  for i = 0, ncals - 1 do begin
    ; file name for this optics configuration
    filename = filepath('calplot_' + strjoin(strtrim(strsplit(ucals[i], ' ', /extract), 2)) + '.eps', $
                        root=plot_dir)
    print, filename
    device, filename=filename

    ; quick and dirty 'naive' demodulation of the data
    comp_cal_comblk_quickdemod, data, ucals[i], stokesi, stokesq, stokesu, stokesv
    pmax = 2.0 * median(stokesi[where(mask)]) ; max intensity for Stokes I image

    ; load black and white color table (for Stokes I)
    tvlct, findgen(256), findgen(256), findgen(256)
    plot_image, stokesi * mask, min=0, max=pmax, title='I '+ucals[i] ; plot stokes I

    tvlct, r, b, r ; load purple-green color table (for Stokes Q)
    plot_image,stokesq/stokesi*mask,min=-.3,max=.3,top=254,bottom=1,title='Q/I '+ucals[i] ; Plot Q.

    ; if there is fitted calibration data (comp_cal_powfunc has been run), plot
    ; that too
    if (n_elements(cal_data) eq n_elements(data)) then begin
      ; quick and dirty demodulation of the fitted calibration data
      comp_cal_comblk_quickdemod, cal_data, ucals[i], stokesic, stokesqc, $
                                  stokesuc, stokesvc
      ; max intensity level for fitted Stokes I
      pmaxc = 2.0 * median(stokesic[where(mask)])

      ; load black and white color table
      tvlct, findgen(256), findgen(256), findgen(256)
      ; plot fitted I
      plot_image, stokesic * mask, min=0, max=pmaxc, $
                  title='I (fit) ' + ucals[i]

      ; load purple-green color table for fitted Stokes Q; plot fit Q (below)
      tvlct, r, b, r
      plot_image, stokesqc / stokesic * mask, min=-.3, max=.3, $
                  top=254, bottom=1, title='Q/I (fit) ' + ucals[i]
    endif
    ; plot Stokes U and V
    plot_image, stokesu / stokesi * mask, min=-.3, max=.3, $
                top=254, bottom=1, title='U/I ' + ucals[i]
    plot_image, stokesv / stokesi * mask, min=-.3, max=.3, $
                top=254, bottom=1, title='V/I ' + ucals[i]
    ; if fits are present, plot fit U and V
    if (n_elements(cal_data) eq n_elements(data)) then begin
      plot_image, stokesuc / stokesic * mask, min=-.3, max=.3, $
                  top=254, bottom=1, title='U/I (fit)' + ucals[i]
      plot_image, stokesvc / stokesic * mask, min=-.3, max=.3, $
                  top=254, bottom=1, title='V/I (fit)' + ucals[i]
    endif
    device, /close
  endfor
end

