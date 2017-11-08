; docformat = 'rst'

;+
; Procedure to calculate normalization factors for comp flat images based on
; the intensity of the solar spectrum.
;
; :Uses:
;   comp_read_atlas, comp_trans
;
; :Params:
;   lambda : in, required, type=fltarr
;     vector of wavelengths
;   t_on : out, optional, type=float
;     on-band transmission
;   t_off : out, optional, type=float
;     off-band transmission
;
; :Author:
;   MLSO Software Team
;-
pro comp_flat_norm, lambda, t_on, t_off
  compile_opt idl2

  ; read 23 angstroms around central wavelength
  dat = comp_read_atlas(lambda * 10., 25.0)

  wave = dat[0, *] / 10.   ; get wavelength scale, convert to nm
  spec = dat[1, *]   ; get spectrum

  ; get comp transmission profiles and plot them
  comp_trans, lambda, wave, trans_on, trans_off

  trans_on = trans_on / total(trans_on)   ; normalize area under filters to 1
  trans_off = trans_off / total(trans_off)

  t_on = total(trans_on * spec)   ; sample spectrum with filter
  t_off = total(trans_off * spec)
end


; Main procedure to test routine to compute normalization factors for comp flat
; images based on the intensity of the solar spectrum sampled by the on band
; and off band filters

;lambda=[1074.38,1074.50,1074.62,1074.74,1074.86]
lambda = [1079.54, 1079.66, 1079.78, 1079.90, 1078.02]
;lambda=[1082.76,1082.88,1083.00,1083.12,1083.24]
nlam = n_elements(lambda)

for i = 0L, nlam - 1L do begin
  comp_flat_norm, lambda[i], t_on, t_off
  print, t_on, t_off, (t_on - t_off)
endfor
end
