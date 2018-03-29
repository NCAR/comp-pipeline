; docformat = 'rst'

;+
; Flatten a list of arrays into a single continuous array. Gets type from
; element arrays in list, they must all match.
;
; :Returns:
;   array
;
; :Params:
;   lst : in, required, type=list
;     list of arrays
;-
function mg_flatten_list, lst
  compile_opt strictarr
  on_error, 2

  n = 0L
  foreach x, lst do begin
    n += n_elements(x)
    type = size(x, /type)
    if (n_elements(first_type) gt 0L) then begin
      if (type ne first_type) then message, 'mismatching types in list'
    endif else first_type = type
  endforeach

  result = make_array(n, type=type)

  i = 0L
  foreach x, lst do begin
    result[i] = x
    i += n_elements(x)
  endforeach

  return, result
end


; main-level example program

lst1 = list([1, 2], [3, 4], [5, 6], [7, 8, 9])
lst2 = list([0, 2], [1, 3])                   

x = lst1[lst2[1]]

f = mg_flatten_list(x)

obj_destroy, x

end
