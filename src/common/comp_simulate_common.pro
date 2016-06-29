;  wavelength -    rest wavelength of emission line (nm)
;  lambda -        wavelength scale for computation (nm)
;  intensity -     maximum intensity of coronal emission line
;  velocity -      doppler shift of line relative to rest wavelength (km/s)
;  line_width -    e-folding half width of emission line (km/s)
;  background -    intensity of background continuum in units of line intensity
;  bfield -        line-of-sight component of magnetic field (G)
;  linear -        percent linear polarization
;  azimuth -       plane-of-sky azimuth of magnetic field (degrees)
;  filters -       array containing filter transmission profiles
;  obs -           array containing observed quantities (4 Stokes,num of tunings)
;  obs_fit -       array containing the fit to observed quantities (4 Stokes,num of tunings)
;  cv -            Stokes V calibration factor, -8.09d-6
;  weights -       fitting weights
common simulate, wavelength, $
                 lambda, $
                 intensity, $
                 velocity, $
                 line_width, $
                 background, $
                 bfield, $
                 linear, $
                 azimuth, $
                 filters, $
                 obs, $
                 obs_fit, $
                 cv, $
                 weights

