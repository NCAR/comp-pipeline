; docformat = 'rst'

;+
; :Author:
;   MLSO Software Team
;-
function comp_diff_iquv, p, stokes_i, stokes_q, stokes_u, stokes_v, requested
  compile_opt strictarr
  @comp_simulate_common

  s = size(obs)
  ntune = s[2]

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

  c = 3e5

  ; this turns out to be a useful quantity to know
  dop_vel_wav = c * (lambda / wavelength - 1.0) - velocity

  dop_vel_wav2 = dop_vel_wav^2
  sinazm = sin(azimuth * !pi / 90.0)
  cosazm = cos(azimuth * !pi / 90.0)

  dp = dblarr(4, ntune, n_elements(p))

  ; the parameters of the fit are:
  ; 0 - intensity
  if (requested[0] ne 0) then begin
    dp[0, *, 0] = obs_fit[0, *] / intensity
    dp[1, *, 0] = linear * cosazm * dp[0, *, 0] * weights[1, *]
    dp[2, *, 0] = linear * sinazm * dp[0, *, 0] * weights[2, *]
    dp[0, *, 0] *= weights[0,*]
    dp[3, *, 0] = (1.0 / intensity * obs_fit[3, *] + v_offset) * weights[3, *]
  endif

  ; 1 - velocity
  if (requested[1] ne 0) then begin
    for i = 0, ntune - 1 do $
      dp[0, i, 1] = 2.0 / line_width^2.0 * total(stokes_i * dop_vel_wav * filters[*, i])

    dp[1, *, 1] = linear * cosazm * dp[0, *, 1] * weights[1, *]
    dp[2, *, 1] = linear * sinazm * dp[0, *, 1] * weights[2, *]
    for i = 0, ntune - 1 do begin
      dp[0, i, 1] *= weights[0, i]
      dp[3, i, 1] = (2.0 * cv * bfield *c / (line_width^2.0 * wavelength) * obs_fit[0, i] - 4.0 * cv * bfield * c / (line_width^4 * wavelength) * total(dop_vel_wav2 * stokes_i * filters[*, i]) - crosstalk * dp[0, i, 1]) * weights[3, i]
    endfor
  endif

  ; 2 - line_width
  if (requested[2] ne 0) then begin
    for i = 0, ntune - 1 do $
      dp[0, i, 2] = 2.0 / line_width^3.0 * total(stokes_i * dop_vel_wav2 * filters[*, i])

    dp[1, *, 2] = linear * cosazm * dp[0, *, 2] * weights[1, *]
    dp[2, *, 2] = linear * sinazm * dp[0, *, 2] * weights[2, *]
    for i = 0, ntune - 1 do begin
      dp[0, i, 2] *= weights[0, i]
      dp[3, i, 2] = (crosstalk * dp[0, i, 2] + 2.0 / line_width * total(stokes_v * (dop_vel_wav2 / line_width^2.0 - 1.0) * filters[*, i])) * weights[3, i]
    endfor
  endif

  ; 3 - bfield
  if (requested[3] ne 0) then $
    for i = 0, ntune - 1 do $
    dp[3, i, 3] = -2.0 * cv * c / wavelength / line_width^2.0 * total(stokes_i * dop_vel_wav * filters[*, i]) * weights[3, i]

  ; tests are too expensive for these parameters, just compute them
  ; 4 - linear
  dp[1, *, 4] = cosazm * obs_fit[0, *] * weights[1, *]
  dp[2, *, 4] = sinazm * obs_fit[0, *] * weights[2, *]

  ; 5 - azimuth
  dp[1, *, 5] = -1.0 * linear * sinazm * !pi / 90.0 * obs_fit[0, *] * weights[1, *]
  dp[2, *, 5] = linear * cosazm * !pi / 90.0 * obs_fit[0, *] * weights[2, *]

  ; 6 - offset in Stokes Q
  dp[1, *, 6] = -1.0 * weights[1, *]

  ; 7 - offset in Stokes U
  dp[2, *, 7] = -1.0 * weights[2, *]

  ; 8 - offset in Stokes V
  dp[3, *, 8] = -1.0 * weights[3, *]

  ; 9 - Stokes U-V crosstalk
  dp[3, *, 9] = -1.0 * obs_fit[0, *] * weights[3, *]

  return, dp
end
