; docformat = 'rst'

;+
; Convert a float to a string suitable for storing in a database. NaN and
; infinite values are converted to "NULL".
;
; :Returns:
;   string
;
; :Params:
;   x : in, required, type=float
;     float to convert
;
; :Keywords:
;   format : in, optional, type=string, default="%f"
;     format to use to convert float to string in C-style format
;-
function comp_db_float2str, x, format=format
  compile_opt strictarr

  if (~finite(x)) then return, 'NULL'

  _format = n_elements(format) eq 0L ? '%f' : format
  _format = '(%"' + _format + '")'

  return, string(x, format=_format)
end
