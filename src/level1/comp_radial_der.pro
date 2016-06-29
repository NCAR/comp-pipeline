; docformat = 'rst'

;+
; Function to interpolate radial scans in an image, take the derivative, and
; fit the maximum with a parabola to find the location of a discontinuity. This
; routine is used to find the location of the solar limb. The variable `nscan`
; below determined the number of radial scans. This routine differentiates
; between positive and negative discontinuities, depending on the input keyword
; `neg_pol`. Positive polarity is the default.
;
; Output:
;   the the array of radial positions is returned (pixels)
;   theta - the of array of angles used (radians)
;
; :Examples:
;   For example, call it like::
;
;     cent = comp_radial_der(data, theta, radius, dr)
;
; :Uses:
;   parabola
;
; :Returns:
;   `dblarr(360)`
;
; :Params:
;   data : in, required, type=fltarr
;     the data image to analyze
;   theta : in, required, type=float
;   radius : in, required, type=float
;     the approximate radius of the discontinuity (pixels)
;   dr : in
;     the region +/- around radius to make the scan (pixels)
;
; :Keywords:
;   neg_pol : in, optional, type=boolean
;     this determines the polarity of the discontinuity, `neg_pol=1` for
;     negative polarity
;
; :Author:
;   Tomczyk
;
; :History:
;   added comments, 10/24/14 ST
;-
function comp_radial_der, data, theta, radius, dr, neg_pol=neg_pol
  compile_opt strictarr
  on_error, 2

  ans = ' '
  debug = 0   ; 1=debug mode on, 0=off

  ; get array dimensions
  s = size(data)
  nx = s[1]
  ny = s[2]

  nscan = 360   ; number of radial scans around circumference
  theta = fltarr(nscan)

  cent = dblarr(nscan)

  ; make initial guess of x and y positions the center of the array
  x0 = double(nx) / 2.D0
  y0 = double(ny) / 2.D0

  nvals = dr * 2   ; number of points in interpolated radial scan

  ;if debug eq 1 then tvwin,data

  ; make radial scans

  for i = 0L, nscan - 1L do begin
    ; angle for radial scan
    theta[i] = double(i) * 2.d0 * !dpi / double(nscan)

    ; x1 and y1 are start x and y coords; x2 and y2 are end coords
    x1 = x0 + (radius - dr) * cos(theta[i])
    y1 = y0 + (radius - dr) * sin(theta[i])
    x2 = x0 + (radius + dr) * cos(theta[i])
    y2 = y0 + (radius + dr) * sin(theta[i])

    ; dx and dy are spacing in x and y
    dx = (x2 - x1) / double(nvals - 1)
    dy = (y2 - y1) / double(nvals - 1)

    ; xx and yy are x and y coords to interpolate onto for radial scan
    xx = dindgen(nvals) * dx + x1
    yy = dindgen(nvals) * dy + y1

    ;if debug eq 1 then plots, xx, yy, color=200, /device

    ; compute radial intensity scan
    rad = interpolate(data, xx, yy, cubic=-0.5, missing=0.0)

    rad = deriv(rad)    ; take derivative of radial intensity scan

    ; change sign if negative polarity
    if (keyword_set(neg_pol)) then rad *= -1.0

    ; find position of maximum derivative, imax
    mx = max(rad, imax)
    if (imax gt nvals - 3) then imax = nvals - 3
    if (imax lt 2) then imax=2

    cent[i] = radius - dr $
                + parabola([double(imax - 1.), $
                            double(imax), $
                            double(imax + 1.)], $
                           [rad[imax - 1], $
                            rad[imax],$
                            rad[imax + 1]])

    if (debug eq 1) then begin
      mg_log, 'theta: %s', strjoin(strtrim(theta, 2), ', '), name='comp', /debug
      plot, rad
      oplot, [cent[i] - radius + dr, cent[i] - radius + dr], [0., 2. * mx]
      read, 'enter return:', ans
    endif
  endfor

  return, cent
end
