; docformat = 'rst'

function comp_cross_min, p
  compile_opt strictarr
  common fit, stokes_i, stokes_q, stokes_u, stokes_v

  corrected_v = stokes_v - p[0] * stokes_i - p[1] * stokes_q - p[2] * stokes_u

  return, total(corrected_v^2)
end