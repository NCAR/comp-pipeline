; docformat = 'rst'

function comp_display_mask, dims, $
                            occulter_radius=occulter_radius, $
                            field_radius=field_radius
  compile_opt strictarr

  mask = bytarr(dims[0], dims[1]) + 1B

  if (n_elements(occulter_radius) gt 0L) then begin
    d = shift(dist(dims[0], dims[1]), dims[0] / 2L, dims[1] / 2L)
    occulter_mask = d gt occulter_radius
    mask and= occulter_mask
  endif

  if (n_elements(field_radius) gt 0L) then begin
    d = shift(dist(dims[0], dims[1]), dims[0] / 2L, dims[1] / 2L)
    field_mask = d lt field_radius
    mask and= field_mask
  endif

  return, mask
end
