; docformat = 'rst'

;+
; Do a quick and dirty 'naive' demodulation (Q=I+Q-(I-Q), etc) using the contents of the
; common block for plotting purposes.

; :Params:
;   datain :, in, required
;     Data array to plot - should have same size as common block data array.
;     handed in so that we can plot both the input data and calibration fit
;     data with the same routine and without a bunch of duplicated code
;     inside 'if' statements.
;   ucal : in, required
;     Which unique calibration optics state to do the demodulation for.
;
; Outputs:
;   stokesi : out, optional
;     Stokes I image
;   stokesq : out, optional
;     Stokes Q image
;   stokesu : out, optional
;     Stokes U image
;   stokesv : out, optional
;     Stokes V image
;
; Joseph Plowman
;-
pro comp_cal_comblk_quickdemod, datain, ucal, stokesi, stokesq, stokesu, stokesv
  compile_opt strictarr

  common comp_cal_comblk, xybasis, xyb_upper, xyb_lower, dataupper, datalower, $
                          varsupper, varslower, xmat, ymat, cpols, pangs, $
                          crets, upols, datapols, datacals, cal_data, $
                          uppercoefs, lowercoefs, uppermask, lowermask, data, $
                          vars, mask, nstokes, ucals, calvars, calvar_solve

  ; pull the parts we're interested in out of the input data array
  ipq = datain[*, *, where(datacals eq ucal and datapols eq 'I+Q')]
  imq = datain[*, *, where(datacals eq ucal and datapols eq 'I-Q')]
  ipu = datain[*, *, where(datacals eq ucal and datapols eq 'I+U')]
  imu = datain[*, *, where(datacals eq ucal and datapols eq 'I-U')]
  ipv = datain[*, *, where(datacals eq ucal and datapols eq 'I+V')]
  imv = datain[*, *, where(datacals eq ucal and datapols eq 'I-V')]

  ; demodulate them
  stokesi = (ipq + imv + ipu + imu + ipv + imv) / 6.0
  stokesq = (ipq-imq)/2.0
  stokesu = (ipu-imu)/2.0
  stokesv = (ipv-imv)/2.0
end
