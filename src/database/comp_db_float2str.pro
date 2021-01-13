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
  on_ioerror, conversion_error

  if (n_elements(x) eq 0L) then return, 'NULL'
  if (~finite(x)) then return, 'NULL'

  _format = n_elements(format) eq 0L ? '%f' : format
  _format = '(%"' + _format + '")'

  result = string(x, format=_format)
  return, result

  conversion_error:
  mg_log, 'error converting \"%s\" to string', strtrim(x, 2), name='comp', /warn
end
