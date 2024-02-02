; docformat = 'rst'

;+
; Compute the rest wavelength.
;
; Returns NaN if no valid east or west pixels.
;
; :Params:
;   primary_header : in, required, type=strarr
;     primary FITS header, needed to produce geometric mask
;   velocity : in, required, type="fltarr(nx, ny)"
;     center lines velocity to compute rest wavelength from
;   intensity : in, required, type="fltarr(nx, ny, 3)"
;     center lines intensity for thresholding
;   line_width : in, required, type="fltarr(nx, ny)"
;     center lines line width for thresholding
;
; :Returns:
;   indices : out, optional, type=lonarr
;     indices into velocity used in calculation, and valid to apply rest
;     wavelength to
;-
function comp_compute_rest_wavelength, primary_header, $
                                       velocity, $
                                       intensity, $
                                       line_width, $
                                       indices=indices
  compile_opt strictarr

  mask = comp_l2_mask(primary_header)

  dims = size(velocity, /dimensions)
  nx = dims[0]
  ny = dims[1]
  x = findgen(nx) - (nx - 1.0) / 2.0
  x = rebin(reform(x, nx, 1), nx, ny)

  threshold_condition = mask gt 0 $
                          and velocity ne 0 $
                          and abs(velocity) lt 30 $
                          and intensity[*, *, 0] gt 0.5 $
                          and intensity[*, *, 1] gt 2.0 $
                          and intensity[*, *, 2] gt 0.5 $
                          and intensity[*, *, 0] lt 60.0 $
                          and intensity[*, *, 1] lt 60.0 $
                          and intensity[*, *, 2] lt 60.0 $
                          and line_width gt 15.0 $
                          and line_width lt 50.0

  indices = where(threshold_condition, /null)

  east = where(threshold_condition and x lt 0.0, n_east, /null)
  west = where(threshold_condition and x gt 0.0, n_west, /null)

  if (method eq 'mean') then begin
    rest_velocity = mean([mean([velocity[east]], /nan), $
                          mean([velocity[west]], /nan)], /nan)
  endif else begin
    rest_velocity = mean([median([velocity[east]]), $
                          median([velocity[west]])], /nan)
  endelse

  return, rest_velocity
end
