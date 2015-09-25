; docformat = 'rst'

;+
; Procedure to fix hot pixels in CoMP images. Replaces data by mean of
; adjacent pixels.
;
; :Uses:
;   comp_config_common
;
; :Params:
;   data : in, required, type=fltarr
;
; :Keywords:
;   hot : in, required, type=lonarr(n)
;     hot pixels
;   adjacent : in, required, type="lonarr(n, 4)"
;     pixels adjacent to hot pixels
;-
function comp_fix_hot, data, hot=hot, adjacent=adjacent
  compile_opt idl2
  @comp_config_common

  fixed = data
  ; TODO: should use /EVEN?
  fixed[hot] = median(data[adjacent], dimension=2)

  return, fixed
end
