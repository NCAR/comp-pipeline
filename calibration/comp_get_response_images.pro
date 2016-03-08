; docformat = 'rst'

;+
; Function to create images which specify the response of each pixel to each
; component of the Stokes vector. Related to the Mueller matrix of the
; polarization analyzer in this configuration, except that we only see
; intensities (scalars) at the detector - it simply sums the intensities of
; each component of the Stokes vector. There are therefore four such images for
; any given configuration of the polarization analyzer; equivalently, there is
; a vector for each pixel location whose inner product with a Stokes vector
; gives the intensity which would be observed by the detector.
;
; :Returns:
;   the set of four images described above
;
; :Params:
;   coefs : in, required, type=dblarr(nstokes * n_basis)
;     The coefficients, as computed by channel_response_coefficients. There
;     should be `nstokes * n_basis` of them, and they vary first over the
;     spatial basis coefficients, then over the stokes coefficients; see the
;     code below for usage.
;   xybasis : in, required, type="dblarr(,, n_basis)"
;     Basis specifying how the polarization response can vary spatially. See
;     `COMP_CAL_XYBASIS`.
;
; :Author:
;   Joseph Plowman
;-
function comp_get_response_images, coefs, xybasis

  nx = n_elements(xybasis[*, 0, 0])
  ny = n_elements(xybasis[0, *, 0])
  n_basis = n_elements(xybasis[0, 0, *])
  nstokes = round(n_elements(coefs) / n_basis)

  images = dblarr(nx, ny, nstokes)
  for i = 0, nstokes - 1 do begin
    for j = 0, n_basis - 1 do begin
      images[*, *, i] += coefs[i * n_basis + j] * xybasis[*, *, j]
    endfor
  endfor

  return, images
end

