;+
; Project     :	STEREO
;
; Name        :	WCS_PROJ_AZP
;
; Purpose     :	Convert intermediate coordinates in AZP projection.
;
; Category    :	FITS, Coordinates, WCS
;
; Explanation :	This routine is called from WCS_GET_COORD to apply the zenithal
;               perspective (AZP) projection to intermediate relative
;               coordinates.
;
; Syntax      :	WCS_PROJ_AZP, WCS, COORD
;
; Examples    :	See WCS_GET_COORD
;
; Inputs      :	WCS = A World Coordinate System structure, from FITSHEAD2WCS.
;               COORD = The intermediate coordinates, relative to the reference
;                       pixel (i.e. CRVAL hasn't been applied yet).
;
; Opt. Inputs :	None.
;
; Outputs     :	The projected coordinates are returned in the COORD array.
;
; Opt. Outputs:	None.
;
; Keywords    :	MISSING    = Value to fill missing values with.  If not passed,
;                            then missing values are filled with IEEE
;                            Not-A-Number (NaN) values.
;
; Calls       :	TAG_EXIST, NTRIM
;
; Common      :	None.
;
; Restrictions:	Because this routine is intended to be called only from
;               WCS_GET_COORD, no error checking is performed.
;
;               This routine is not guaranteed to work correctly if the
;               projection parameters are non-standard.
;
; Side effects:	None.
;
; Prev. Hist. :	None.
;
; History     :	Version 1, 25-Apr-2005, William Thompson, GSFC
;               Version 2, 19-May-2005, William Thompson, GSFC
;                       Made more efficient when gamma=0
;               Version 3, 21-Jun-2005, William Thompson, GSFC
;                       Improved test for theta selection
;
; Contact     :	WTHOMPSON
;-
;
pro wcs_proj_azp, wcs, coord, missing=k_missing
on_error, 2
halfpi = !dpi / 2.d0
;
;  Get the MISSING value.
;
if n_elements(k_missing) eq 1 then missing=k_missing else missing=!values.d_nan
;
;  Calculate the conversion from coordinate units into radians.
;
cx = !dpi / 180.d0
case wcs.cunit[wcs.ix] of
    'arcmin': cx = cx / 60.d0
    'arcsec': cx = cx / 3600.d0
    'mas':    cx = cx / 3600.d3
    'rad':    cx = 1.d0
    else:     cx = cx
endcase
;
cy = !dpi / 180.d0
case wcs.cunit[wcs.iy] of
    'arcmin': cy = cy / 60.d0
    'arcsec': cy = cy / 3600.d0
    'mas':    cy = cy / 3600.d3
    'rad':    cy = 1.d0
    else:     cy = cy
endcase
;
;  Get the native longitude (phi0) and latitude (theta0) of the fiducial
;  point.  Look for the PV values from the FITS header.  If not found, use the
;  default values (0,90).
;
phi0 = 0.d0
if tag_exist(wcs, 'proj_names', /top_level) then begin
    name = 'PV' + ntrim(wcs.ix+1) + '_1'
    w = where(wcs.proj_names eq name, count)
    if count gt 0 then phi0 = wcs.proj_values[w[0]]
endif
;
theta0 = 90.d0
if tag_exist(wcs, 'proj_names', /top_level) then begin
    name = 'PV' + ntrim(wcs.ix+1) + '_2'
    w = where(wcs.proj_names eq name, count)
    if count gt 0 then theta0 = wcs.proj_values[w[0]]
endif
;
;  If PHI0 and THETA0 are non-standard, then signal an error.
;
if (phi0 ne 0) or (theta0 ne 90) then message, /informational, $
      'Non-standard PVi_1 and/or PVi_2 values -- ignored'
;
;  Convert phi0 and theta0 to radians
;
phi0   = (!dpi / 180.d0) * phi0
theta0 = (!dpi / 180.d0) * theta0
;
;  Get the projection source distance mu, and the slant angle gamma.  If not
;  found, use the default values of 0.
;
mu = 0.d0
if tag_exist(wcs, 'proj_names', /top_level) then begin
    name = 'PV' + ntrim(wcs.iy+1) + '_1'
    w = where(wcs.proj_names eq name, count)
    if count gt 0 then mu = wcs.proj_values[w[0]]
endif
;
gamma = 0.d0
if tag_exist(wcs, 'proj_names', /top_level) then begin
    name = 'PV' + ntrim(wcs.iy+1) + '_2'
    w = where(wcs.proj_names eq name, count)
    if count gt 0 then gamma = wcs.proj_values[w[0]]
endif
;
;  Convert gamma to radians.
;
gamma = (!dpi / 180.d0) * gamma
;
;  Get the celestial longitude and latitude of the fiducial point.
;
alpha0 = wcs.crval[wcs.ix] * cx
delta0 = wcs.crval[wcs.iy] * cy
;
;  Get the native longitude (phip) of the celestial pole.  Look for the LONPOLE
;  (or PVi_3) keyword.  If not found, use the default value.  Convert to
;  radians.
;
if delta0 ge theta0 then phip=0 else phip=180
if tag_exist(wcs, 'proj_names', /top_level) then begin
    w = where(wcs.proj_names eq 'LONPOLE', count)
    if count gt 0 then phip = wcs.proj_values[w[0]]
    name = 'PV' + ntrim(wcs.ix+1) + '_3'
    w = where(wcs.proj_names eq name, count)
    if count gt 0 then phip = wcs.proj_values[w[0]]
endif
if (phip ne 180) and (delta0 ne halfpi) then message, /informational, $
  'Non-standard LONPOLE value ' + ntrim(phip)
phip   = (!dpi / 180.d0) * phip
;
;  Calculate the native spherical coordinates.
;
phi = atan(cx*coord[wcs.ix,*],-cy*coord[wcs.iy,*]*cos(gamma))
r_theta = sqrt((cx*coord[wcs.ix,*])^2 + (cy*coord[wcs.iy,*]*cos(gamma))^2)
if gamma eq 0 then rho = r_theta / (mu + 1) else $
  rho = r_theta / (mu + 1 + cy*coord[wcs.iy,*]*sin(gamma))
psi = atan(1,rho)
omega = rho*mu/sqrt(rho^2+1)
w_missing = where(abs(omega) gt 1, n_missing)
omega = asin(-1>omega<1)
theta = psi - omega
w = where(theta gt !dpi, count)
if count gt 0 then theta[w] = theta[w] - (2.d0*!dpi)
theta2 = psi + omega + !dpi
w = where(theta2 gt !dpi, count)
if count gt 0 then theta2[w] = theta2[w] - (2.d0*!dpi)
w = where(((abs(theta2-halfpi) lt abs(theta-halfpi)) and $
           (abs(theta2) le halfpi)) or (abs(theta) gt halfpi), count)
if count gt 0 then theta[w] = theta2[w]
;
;  Calculate the celestial spherical coordinates.
;
if delta0 ge halfpi then begin
    alpha = alpha0 + phi - phip - !dpi
    delta = theta
end else if delta0 le -halfpi then begin
    alpha = alpha0 - phi + phip
    delta = -theta
end else begin
    dphi = phi - phip
    cos_dphi = cos(dphi)
    sin_theta = sin(theta)
    cos_theta = cos(theta)
    alpha = alpha0 + atan(-cos_theta*sin(dphi), $
        sin_theta*cos(delta0)-cos_theta*sin(delta0)*cos_dphi)
    delta = asin(sin_theta*sin(delta0) + $
                 cos_theta*cos(delta0)*cos_dphi)
endelse
;
;  Convert back into the original units.
;
coord[wcs.ix,*] = alpha / cx
coord[wcs.iy,*] = delta / cy
;
;  Flag any missing values.
;
if n_missing gt 0 then begin
    coord[wcs.ix, w_missing] = missing
    coord[wcs.iy, w_missing] = missing
endif
;
end
