;+
; NAME:
;	COMP_UNIQ
;
; PURPOSE:
;	Return the subscripts of the unique elements in an array.
;
;	Note that repeated elements must be adjacent in order to be
;	found.  This routine is intended to be used with the SORT
;	function.  See the discussion of the IDX argument below.
;
;	This command is inspired by the Unix uniq(1) command.
;
; CATEGORY:
;	Array manipulation.
;
; CALLING SEQUENCE:
;	COMP_UNIQ(Array [, Idx] [/first] )
;
; INPUTS:
;	Array:	The array to be scanned.  The type and number of dimensions
;		of the array are not important.  The array must be sorted
;		into monotonic order unless the optional parameter Idx is
;		supplied.
;
; OPTIONAL INPUT PARAMETERS:
;	IDX:	This optional parameter is an array of indices into Array
;		that order the elements into monotonic order.
;		That is, the expression:
;
;			Array(Idx)
;
;		yields an array in which the elements of Array are
;		rearranged into monotonic order.  If the array is not
;		already in monotonic order, use the command:
;
;			COMP_UNIQ(Array, SORT(Array))
;
;		The expression below finds the unique elements of an unsorted
;		array:
;
;			Array(COMP_UNIQ(Array, SORT(Array)))
;
; OUTPUTS:
;	An array of indicies into ARRAY is returned.  The expression:
;
;		ARRAY(COMP_UNIQ(ARRAY))
;
;	will be a copy of the sorted Array with duplicate adjacent
;	elements removed.
;
; Optional Keyword Parameter:
;   first - if set, return index of FIRST occurence for duplicates
;           (default is LAST occurence)
;
; COMMON BLOCKS:
;	None.
;
; MODIFICATION HISTORY:
;	29 July 1992, ACY - Corrected for case of all elements the same.
;       30 Aug  1994, SLF - added /first keyword
;	 1 Sep  1994, MDM - Modified to return a vector for the case of
;			    a single element being returned (so it matches
;			    the pre IDL Ver 3.0 version of UNIQ)
;		           - Modified to return [0] for a scalar
;       10 Sep  1996, Zarro
;                         - modified to return 0 for a scalar and a scalar
;                           for single element being returned.
;       10 Oct  1996, Zarro
;                         - added OLDWAY keyword to return,[value] for scalar
;                           value
;-

function comp_uniq, ARRAY, IDX, FIRST=FIRST,OLDWAY=OLDWAY

; Check the arguments.

  oldway=keyword_set(oldway)
  s = size(ARRAY)
  first=keyword_set(first)

  if (s(0) eq 0) then begin
   val=0
   if oldway then val=[val]
   return,val
  endif

  shifts=([-1,1])(first)   ;slf - shift direction -> first/last
  if n_params() ge 2 then begin		;IDX supplied?
   q = array(idx)
   indices = where(q ne shift(q,shifts), count)
   if (count GT 0) then return, idx(indices) else begin
    val=(n_elements(q)-1) * (1-first)
    if oldway then val=[val]
    return,val
   endelse
  endif else begin
   indices = where(array ne shift(array, shifts), count)
   if (count GT 0) then return, indices else begin
    val=(n_elements(ARRAY)-1) * (1-first)
    if oldway then val=[val]
    return,val
   endelse
  endelse

  end