; docformat = 'rst'

;+
; Wrapper for `LABEL_REGION` that handles 1-dimensional arrays of any size `n`.
;
; :Return:
;   `lonarr(n)`
;
; :Params:
;   values : in, required, type=numeric array
;     array of size `n`
;-
function comp_label_series, values
  compile_opt strictarr

  case n_elements(values) of
    0: return, !null
    1: return, [1L]
    2: begin
        if (values[0] eq values[1]) then begin
          return, lonarr(2)
        endif else begin
          return, [1L, 2L]
        endelse
      end
    else: return, label_region(values)
  endcase
end
