; docformat = 'rst'

;+
; Callback for fitting function in `COMP_ANALYZE`.
;-
function comp_fit_iquv, p, dp
  compile_opt strictarr
  @comp_simulate_common

  s = size(obs)
  ntune = s[2]

  ; array of weights (I, Q, U, V by wavelengths)
  weights = [[1.0, 1.0, 1.0, 1.0], $
             [1.0, 1.0, 1.0, 1.0], $
             [1.0, 1.0, 1.0, 1.0], $
             [1.0, 1.0, 1.0, 1.0], $
             [1.0, 1.0, 1.0, 1.0]]
    
  intensity  = p[0]
  velocity   = p[1]
  line_width = p[2]
  bfield     = p[3]
  linear     = p[4]
  azimuth    = p[5]
  q_offset   = p[6]
  u_offset   = p[7]
  v_offset   = p[8]
  crosstalk  = p[9]

  comp_create_stokes, stokes_i, stokes_q, stokes_u, stokes_v
  stokes_i = stokes_i
  stokes_q = stokes_q - q_offset   ; remove q offset
  stokes_u = stokes_u - u_offset   ; remove u offset
  ; remove v offset and i, q, u to v crosstalk
  stokes_v = stokes_v - v_offset - stokes_i * crosstalk
  
  ; compute comp observed quantities, 4 Stokes parameters at each tuning
  
  obs_fit = fltarr(4, ntune, /nozero)
  for i = 0, ntune - 1 do begin
    obs_fit[0, i] = total(stokes_i * filters[*, i])
    obs_fit[1, i] = total(stokes_q * filters[*, i])
    obs_fit[2, i] = total(stokes_u * filters[*, i])
    obs_fit[3, i] = total(stokes_v * filters[*, i])
  endfor
 
  if (n_params() gt 1) then $
    dp = comp_diff_iquv(p, stokes_i, stokes_q, stokes_u, stokes_v, dp)
  
  ; for POWELL, compute weighted chi-squared
  ;result = total(weights * (obs_fit - obs)^2)

  ; for MPFIT, return weighted difference
  result = reform(weights * (obs - obs_fit), 20)

  return, result
end
