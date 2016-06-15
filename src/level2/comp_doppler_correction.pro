; docformat = 'rst'

;+
; Do doppler correction.
;
; :Uses:
;   comp_constants_common, fmedian
;
; :Params:
;   fit_arr_in : in, required, type="array[nx, ny, 2]"
;     input fitting parameters
;   fit_arr_out : out, optional, type="array[nx, ny, 2]"
;     output fitting parameters after Doppler shift correction
;   wave_type : in, required, type=string
;     wave type, '1074', '1079', or '1083'
;   ewtrend : out, optional, type=array[nx]
;     output parameter, east-west trend of the Doppler shift
;   temptrend : out, optional, type=float
;     output parameter, temporal trend of the Doppler shift
;
; :Author:
;   Hui Tian
;
; :History:
;   Hui Tian, March 28, 2012, for CoMP Doppler shift correction
;-
pro comp_doppler_correction, fit_arr_in, fit_arr_out, wave_type, ewtrend, $
                             temptrend
  compile_opt strictarr
  @comp_constants_common

  case wave_type of
   '1074': rest = double(center1074)
   '1079': rest = double(center1079)
   '1083': rest = double(center1083)
  endcase
  c = 299792.458D

  nx = (size(fit_arr_in))[1]
  ny = (size(fit_arr_in))[2]
  x  = dindgen(nx) - nx / 2   ; in pixels
  ewtrend = fltarr(nx)
  temptrend = 0.
  fit_arr_out = fit_arr_in
  residualtrend = fltarr(nx)
  for i = 0L, nx - 1L do begin
    ; exclude pixels with no data or low S/N data and also bad velocity data
    sub_good = where(fit_arr_in[i, *, 0] ge 2 and fit_arr_in[i, *, 1] gt 0, $
                     n_good)
    if (n_good ne 0) then begin
      residualtrend[i] = median(fit_arr_in[i, sub_good, 1])
    endif else residualtrend[i] = rest
  endfor

  sub_abnor = where(abs(residualtrend - rest) ge 0.4)
  if (sub_abnor[0] ne -1) then residualtrend[sub_abnor] = rest
  sub_eff = where(residualtrend ne rest, n_eff)
  s1 = sub_eff[0]
  s2 = sub_eff[n_eff - 1]
  ; median filter of the east-west trend
  residualtrend = fmedian(residualtrend, 20)
  if (size(x[s1:s2]))[1] gt 1 then begin
    ; 5th order polynomial fit to the east-west trend, 30:590
    r = poly_fit(x[s1:s2], residualtrend[s1:s2], 5, /double)
    ewtrend = poly(x, r)
    resitren = reform(ewtrend) # (fltarr(ny) + 1)
    ; eliminate the east-west trend
    fit_arr_in[*, *, 1] = reform(fit_arr_in[*, *, 1]) - resitren
  endif
  sub = where(reform(fit_arr_in[*,*,0]) ge 2) ;exclude pixels with no data or low S/N data
  if ((size(sub))[0] eq 1) then begin
    ; may select proper ranges to avoid eruptive events, e.g.,
    ; v1=reform(fit_arr_in[300:610,0:619,1,t])
    v1 = reform(fit_arr_in[*, *, 1])
    v1_corr = median(v1[sub])   ; mean(v1(sub), /double)
    temptrend = v1_corr
  endif else begin
    v1_corr = 0.
    temptrend = rest
  endelse

  ; eliminate the temporal trend
  fit_arr_out[*, *, 1] = (fit_arr_in[*, *, 1] - v1_corr) / rest * c
end

