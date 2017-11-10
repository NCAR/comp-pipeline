; docformat = 'rst'

;+
; Retrieve the NDFILTER for an extension.
;
; :Returns:
;   integer
;
; :Params:
;   date : in, required, type=string
;     date in the form "YYYYMMDD"
;   wave_type : in, required, type=string
;     wavelength type, i.e., "1074", "1079", or "1083"
;   header : in, required, type=strarr
;     extension header
;
; :Author:
;   MLSO Software Team
;-
function comp_get_nd_filter, date, wave_type, header
  compile_opt strictarr

  nd_filter = sxpar(header, 'NDFILTER', count=nd_filter_present)
  if (nd_filter_present eq 0) then nd_filter = 8
  return, nd_filter
end
