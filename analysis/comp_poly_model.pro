; docformat = 'rst'

function comp_poly_model, x, p, dp
  compile_opt strictarr

  return, poly(x, p)
end
