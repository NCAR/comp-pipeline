; docformat = 'rst'

;+
; Find the indices of the given wavelengths corresponding to the required 3 pt
; wavelengths.
;
; :Returns:
;   `lonarr(3)`
;
; :Params:
;   wave_type : in, required, type=string
;     wave type, i.e., '1074', '1079', or '1083'
;   wavelengths : in, required, type=fltarr
;     wavelengths in which to find the required wavelengths
;
; :Keywords:
;   tolerance : in, optional, type=float, default=0.01
;     tolerance to find wavelengths; if a wavelength is not found with in the
;     tolerance, it is considered an error
;   error : out, optional, type=long
;     set to a named variable to retrieve whether the required wavelengths were
;     found
;-
function comp_3pt_indices, wave_type, wavelengths, tolerance=tolerance, error=error
  compile_opt strictarr
  @comp_constants_common

  error = 0L

  _tolerance = n_elements(tolerance) eq 0L ? 0.01 : tolerance
  case wave_type of
    '1074': wave_3pt = wavelengths_3pt_1074
    '1079': wave_3pt = wavelengths_3pt_1079
    '1083': wave_3pt = wavelengths_3pt_1083
  endcase

  indices = lonarr(3)
  for i = 0L, 2L do begin
    min_value = min(abs(wave_3pt[i] - wavelengths), ind)
    indices[i] = ind
    if (min_value gt tolerance) then error = 1L
  endfor

  return, indices
end
