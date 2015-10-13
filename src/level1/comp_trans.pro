; docformat = 'rst'

;+
; Function to return the on-band and off-band transmission of the comp filter
; given the central wavelength, lambda (nm) and the wavelength scale (nm).
;-
pro comp_trans, lambda, wave, trans_on, trans_off
  compile_opt strictarr

  trans_on = cos(!pi * (wave - lambda) / 2.3)^2 $
               * cos(2. * !pi * (wave - lambda) / 2.3)^2 $
               * cos(4. * !pi * (wave - lambda) / 2.3)^2 $
               * cos(8. * !pi * (wave - lambda) / 2.3)^2

  trans_off = cos(!pi * (wave - lambda) / 2.3)^2 $
                * sin(2. * !pi * (wave - lambda) / 2.3)^2 $
                * cos(4. * !pi * (wave - lambda) / 2.3)^2 $
                * cos(8. * !pi * (wave - lambda) / 2.3)^2
end