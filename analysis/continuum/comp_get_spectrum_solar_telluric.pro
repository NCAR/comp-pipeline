; docformat = 'rst'

;+  
; Procedure to read in the solar spectrum from the KPNO solar atlas (NSO Atlas
; #1) and separate telluric and solar contributions to the spectrum. This
; routine only works currently in the vicinity of the FeXIII 1074 and 1079
; coronal emission lines.
;  
; :Params:
;   lam0 : in, required, type=float
;      wavelength of spectral region (nm) (e.g. 1074.7) 
;   lambda : out, optional, type=dblarr
;       wavelength scale of the solar and telluric spectra (nm)
;   solar_spec : out, optional, type=dblarr
;     solar spectrum in this region (continuum normalized)
;   telluric_spec : out, optional, type=dblarr
;     telluric spectrum in this region (continuum normalized)
;
; :Uses:
;   comp_read_atlas
;
; :Author:
;   Tomczyk
;-
pro comp_get_spectrum_solar_telluric, lam0, lambda, solar_spec, telluric_spec
  compile_opt strictarr

  ; read spectrum
  dat = comp_read_atlas(lam0 * 10.0, 20.0)

  ; get wavelength scale for atlas spectrum, convert to nm
  lambda = double(dat[0, *] / 10.0)
  spec   = double(dat[1, *])   ; get atlas spectrum
  nlam   = n_elements(lambda)

  ; Make assignment of wavelengths and line widths for lines in
  ; region. Identifications, wavelengths and line widths come from Swensson,
  ; Benedict, Delbouille and Roland.
  if (abs(lam0 - 1074.7) lt abs(lam0 - 1079.8)) then begin   ; check if 1074 or 1079
    line_wave = [1073.757, 1074.180, 1074.347, 1074.467, 1074.615, 1074.647, $
                 1074.939, 1075.124, 1075.303, 1075.403, 1075.478]
    id = ['h2o', 'solar', 'h2o', 'h2o', 'h2o', 'solar', $
          'solar', 'h2o', 'solar', 'solar', 'solar']
    width = [0.04, 0.10, 0.10, 0.10, 0.04, 0.04, $
             0.27, 0.04, 0.08, 0.08, 0.04]
  endif else begin
    line_wave = [1079.147, 1079.217, 1079.515, 1079.611, 1079.720, 1079.958, $
                 1080.134, 1080.369, 1080.643]
    ; potential bug: first 'solar' was 'solar ', setting solar_spec instead of
    ; telluric_spec below
    id = ['solar', 'h2o', 'h2o', 'solar', 'solar', 'h2o', $
          'solar', 'h2o', 'h2o']
    width = [0.04, 0.04, 0.08, 0.10, 0.04, 0.24, 0.08, 0.14, 0.10]
  endelse

  ; snip out solar and telluric features. There are two ways to fill the snipped
  ; region. Either fill with a constant value or interpolate over region using
  ; endpoints.
  solar_spec    = spec
  telluric_spec = spec

  nlines = n_elements(line_wave)
  for i = 0, nlines - 1 do begin
    snip = where(lambda lt line_wave[i] + width[i] / 2.0 $
                   and lambda gt line_wave[i] - width[i] / 2.0)
    i1 = min(snip)
    i2 = max(snip)
    
    ; put constant maximum value of region into snipped region  
    if (id[i] eq 'solar') then begin
      telluric_spec[snip] = max(solar_spec[snip])
    endif else begin
      solar_spec[snip] = max(solar_spec[snip])
    endelse

    ; enter mean of endpoints into snipped region
    ;v = (solar_spec[i1] + solar_spec[i2]) /2.0
    ;if (id[i] eq 'solar') then telluric_spec[snip] = v else solar_spec[snip] = v

    ; interpolate over snipped region
    ;lam1=lambda[i1] & lam2=lambda[i2]   
    ;s1=spec[i1] & s2=spec[i2]
    ;a=(s2-s1)/(lam2-lam1)   ;slope
    ;b=s1-a*lam1
    ;if (id[i] eq 'solar') then begin
    ;  telluric_spec[snip]=a*lambda[snip]+b
    ;endif else begin
    ;  solar_spec[snip]=a*lambda[snip]+b
    ;endelse
  endfor
  
  ; modify telluric spectrum if h2o_factor keyword is set
  ;if n_elements(h2o_factor) eq 1 then begin
  ;  telluric_spec=( 1.0 - (1.0-telluric_spec)*h2o_factor ) > 0.
  ;endif
end