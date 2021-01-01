; docformat = 'rst'

;+
; Update configuration values that depend on the date, i.e., `process_basedir`
; and `raw_basedir` (if `process_routing_filename` or `raw_routing_filename`
; are present)
;
; :Params:
;   date : in, required, type=string
;     date to update the configuration parameters for, in the form "YYYYMMDD"
;-
pro comp_update_configuration, date
  compile_opt strictarr
  @comp_config_common

  if (n_elements(routing_filename) gt 0L) then begin
    _process_basedir = comp_get_route(routing_filename, date, 'process', found=found)
    if (found) then process_basedir = _process_basedir
  endif

  if (n_elements(routing_filename) gt 0L) then begin
    _raw_basedir = comp_get_route(routing_filename, date, 'raw', found=found)
    if (found) then raw_basedir = _raw_basedir
  endif
end
