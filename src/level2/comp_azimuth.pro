; docformat = 'rst'

;+
; Compute the azimuth.
;
; :Returns:
;   fltarr
;
; :Params:
;   u : in, required, type=fltarr
;     Stokes U
;   q : in, required, type=fltarr
;     Stokes Q
;   p_angle : in, required, type=float
;     p angle
;
; :Keywords:
;    radial_azimuth : out, optional, type=fltarr
;      set to a named variable to retrieve the radial azimuth
;-
function comp_azimuth, u, q, p_angle, radial_azimuth=radial_azimuth
  compile_opt strictarr
  @comp_constants_common

  ; compute azimuth and adjust for p-angle, correct azimuth for quadrants
  azimuth = 0.5 * atan(u, q) * 180.0 / !pi - p_angle + 45.0
  azimuth mod= 180.0
  bad = where(azimuth lt 0., count)
  if (count gt 0) then azimuth[bad] += 180.0

  if (arg_present(radial_azimuth)) then begin
    x = rebin(findgen(nx) - float(nx) / 2.0, nx, nx)
    y = transpose(x)
    ; compute theta and convert to degrees
    theta = atan(y, x) * 180.0 / !pi + 180.0
    theta mod= 180.0

    thew = theta + 90.0
    testsouth = where(theta gt 90.0D and theta le 270.0D, count)
    if (count gt 0L) then thew[testsouth] -= 180.0D
    testquad = where(theta gt 270.0D, count)
    if (count gt 0) then thew[testquad] -= 360.0D

    radial_azimuth = azimuth - thew
    test = where(radial_azimuth lt 0., count)
    if (count gt 0) then radial_azimuth[test] += 180.d0
    radial_azimuth -= 90.0
  endif

  return, azimuth
end
