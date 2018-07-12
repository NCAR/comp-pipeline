; docformat = 'rst'

;+
; Determine the central wavelength for a set of wavelengths.
;
; :Returns:
;   float or string (if `NAME` is set)
;
; :Params:
;   wavelengths : in, required, type=fltarr
;     array of wavelengths
;
; :Keywords:
;   name : in, optional, type=boolean
;     set to return the string name "1074", "1079", or "1083" instead
;     of the exact float value
;
; :Author:
;   MLSO Software Team
;-
function comp_find_wave_type, wavelengths, name=name
  compile_opt strictarr
  @comp_constants_common

  regions = [center1074, center1079, center1083]

  m = min(abs(mean(wavelengths) - regions), region_index)
  central_wavelength = regions[region_index]

  return, keyword_set(name) $
            ? strtrim(long(central_wavelength), 2) $
            : central_wavelength
end
