; docformat = 'idl'

;+
; Return the subscripts of the unique elements in an array.
;
; Note that repeated elements must be adjacent in order to be
; found.  This routine is intended to be used with the SORT
; function.  See the discussion of the IDX argument below.
;
; This command is inspired by the Unix uniq(1) command.
;
; :Returns:
;   An array of indicies into ARRAY is returned. The expression::
;
;     array(comp_uniq(array))
;
;   will be a copy of the sorted Array with duplicate adjacent
;   elements removed.
;
; :Params:
;   array : in, required, type=array 
;     The array to be scanned. The type and number of dimensions of the array
;     are not important. The array must be sorted into monotonic order unless
;     the optional parameter `idx` is supplied.
;
;   idx : in, optional, type=index array
;     This optional parameter is an array of indices into `array` that order the
;     elements into monotonic order. That is, the expression::
;
;       array[idx]
;
;     yields an array in which the elements of `array` are rearranged into
;     monotonic order. If the array is not already in monotonic order, use the
;     command::
;
;       comp_uniq(array, sort(array))
;
;     The expression below finds the unique elements of an unsorted array::
;
;       array(comp_uniq(array, sort(array)))
;
; :Keywords:
;   first : in optional, type=boolean
;     if set, return index of first occurence for duplicates (default is last
;     occurence)
;   oldway : in, optional, type=boolean
;     if set, return array of size 1 instead of scalar for 1-element results
;
; :History:
;   29 July 1992, ACY     - Corrected for case of all elements the same.
;   30 Aug  1994, SLF     - added /first keyword
;   1 Sep  1994, MDM      - Modified to return a vector for the case of
;                           a single element being returned (so it matches
;                           the pre IDL Ver 3.0 version of UNIQ)
;                         - Modified to return [0] for a scalar
;   10 Sep  1996, Zarro   - modified to return 0 for a scalar and a scalar
;                           for single element being returned.
;   10 Oct  1996, Zarro   - added OLDWAY keyword to return,[value] for scalar
;                           value
;   see git log for recent changes
;
; :Author:
;   MLSO Software Team
;-
function comp_uniq, array, idx, first=first, oldway=oldway
  compile_opt strictarr

  ; check the arguments
  oldway = keyword_set(oldway)
  s = size(array)
  first = keyword_set(first)

  if (s[0] eq 0) then begin
    val = 0
    if (oldway) then val = [val]
    return, val
  endif

  shifts = ([-1, 1])[first]   ; slf - shift direction -> first/last
  if (n_params() ge 2) then begin   ; idx supplied?
    q = array[idx]
    indices = where(q ne shift(q, shifts), count)
    if (count gt 0) then return, idx[indices] else begin
      val = (n_elements(q) - 1L) * (1 - first)
      if (oldway) then val = [val]
      return, val
    endelse
  endif else begin
    indices = where(array ne shift(array, shifts), count)
    if (count gt 0) then return, indices else begin
      val = (n_elements(array) - 1L) * (1 - first)
      if (oldway) then val = [val]
      return, val
    endelse
  endelse
end