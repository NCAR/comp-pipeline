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
; :Keywords:
;   error : out, optional, type=long
;     set to a named variable to retrieve whether NDFILTER was found and valid;
;     is 0 if no problems
;
; :Author:
;   MLSO Software Team
;-
function comp_get_nd_filter, date, wave_type, header, error=error
  compile_opt strictarr
  on_ioerror, conversion_error
  @comp_constants_common

  error = 0L

  if (wave_type eq '1074' || wave_type eq '1079') then begin
    if (~use_fe_ndfilter) then return, default_ndfilter
  endif

  nd_filter = sxpar(header, 'NDFILTER', count=nd_filter_present)

  if (nd_filter_present eq 0 || (nd_filter_present && ~finite(nd_filter))) then begin
      if (wave_type eq '1083') then begin
        ; TODO: if this is a flat, need to determine likely NDFILTER value if
        ;       not present, i.e., compare 1083 flat mean vs. 1074 flat mean
        ;       because we know 1074 uses 8 (clear)
        ;       if this is a science, we don't know -- use 4
        nd_filter = 4
      endif else begin
        nd_filter = 8
      endelse
  endif

  ; if this conversion has a problem, default_ndfilter will be returned
  if (size(nd_filter, /type) eq 7) then nd_filter = long(nd_filter)

  if (nd_filter lt 1 || nd_filter gt 8) then begin
    error = 1L
    nd_filter = default_ndfilter
  endif

  return, nd_filter

  conversion_error:
  error = 1L
  return, default_ndfilter
end
