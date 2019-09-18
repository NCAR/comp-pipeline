; docformat = 'rst'

;+
; Extract a radial intensity profile from a FITS file.
;
; :Params:
;
; :Keywords:
;   standard_deviation : out, optional, type=fltarr
;     set to a named variable to retrieve the standard deviation of intensity
;     profile
;-
function comp_extract_radial_values, image, radii, sun_pixels, $
                                     cx=cx, cy=cy, $
                                     standard_deviation=standard_deviation, $
                                     limb=limb
  compile_opt strictarr
  on_error, 2

  dims = size(image, /dimensions)
  _cx = n_elements(cx) eq 0L ? (dims[0] - 1) / 2.0 : cx
  _cy = n_elements(cy) eq 0L ? (dims[1] - 1) / 2.0 : cy

  ; angles for full circle in radians
  n_theta = 360L
  theta = findgen(n_theta) * !dtor
  case 1 of
    n_elements(limb) eq 0L: limb_indices = lindgen(n_theta)
    strlowcase(limb) eq 'east': limb_indices = lindgen(n_theta / 2) + n_theta / 4L
    strlowcase(limb) eq 'west': limb_indices = [lindgen(n_theta / 4), $
                                                lindgen(n_theta / 4) + 3 * n_theta / 4L]
    else: message, string(limb, format='(%"unknown limb %s")')
  endcase

  n_radii = n_elements(radii)
  intensity = fltarr(n_radii)
  standard_deviation = fltarr(n_radii)

  for r = 0L, n_radii - 1L do begin
    x = sun_pixels * radii[r] * cos(theta[limb_indices]) + _cx
    y = sun_pixels * radii[r] * sin(theta[limb_indices]) + _cy
    intensity[r] = mean(image[round(x), round(y)])
    standard_deviation[r] = stddev(image[round(x), round(y)])
  endfor

  return, intensity
end
