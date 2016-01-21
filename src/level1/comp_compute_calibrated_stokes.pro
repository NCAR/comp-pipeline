; docformat = 'rst'

;+
; Compute demodulated stokes vector from a set of flat and dark corrected
; Stokes images.
;
; :Returns:
;  Returns a set of images for the demodulated Stokes components as
;  `fltarr(nx, ny, n_stokes)`.
;
; :Params:
;   coef_images : in, required, type="fltarr(nx, ny, nstates, nstokes)"
;     Images of the calibration coefficients
;   data : in, required, type="fltarr(nx, ny, nstates)"
;     Data images, one for each observed polarimeter state. Ordering of nstates
;     in data and vars must match ordering in coef_images.
;   vars : in, required, type="fltarr(nx, ny, nstates)"
;     Variances corresponding to data
;
; :Author:
;   Joseph Plowman
;-
function comp_compute_calibrated_stokes, coef_images, data, vars
  compile_opt strictarr

  nx            = n_elements(data[*, 0, 0])
  ny            = n_elements(data[0, *, 0])
  nstates       = n_elements(data[0, 0, *])
  nstokes       = n_elements(coef_images[0, 0, 0, *])
  amat          = dblarr(nstokes, nstokes)
  bvec          = dblarr(nstokes)
  stokes_images = dblarr(nx, ny, nstokes)

  for x = 0, nx - 1 do begin
    for y = 0, ny - 1 do begin
      for j = 0, nstokes - 1 do begin
        for k = 0, nstokes - 1 do begin
          ; equation 30 from writeup
          amat[j, k] = total(coef_images[x, y, *, j] * coef_images[x, y, *, k] / vars[x, y, *])
        endfor
        ; equation 31 from writeup
        bvec[j] = total(data[x, y, *] * coef_images[x, y, *, j] / vars[x, y, *])
      endfor
      ; equation 32 from writeup
      stokes_images[x, y, *] = invert(amat, /double) # bvec
    endfor
  endfor

  return, stokes_images
end

