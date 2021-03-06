function wc_where_arr, in_array, in_pattern_array, count, $
           case_ignore=case_ignore, NOTEQUAL=notequal
;
;+
;   Name: wc_where_arr
; 
;   Purpose: return subscripts of input array where a pattern match is
;	     found - allows use of multiple wild card characters (*),
;	     works for pattern arrays
;
;            this is an extension of wc_where
;
;   Input Paramters:
;      inarray - string array to search
;      inpattern_array - string (scalar or array) to match - may include wild cards (*)
;
;   Output:
;      function returns subscripts of inarray where match(es) found (-1 if none)
;      mcount - number of matches found 
;
;   Calling Examples:
;      IDL> stringarr = ['hello', 'guys', 'how', 'are', 'you', 'doing', 'today', '?']
;      IDL> print, wc_where( stringarr, 'how' )
;                 2
;      IDL> print, wc_where( stringarr, ['how', 'today'] )
;                 2
;      IDL> print, wc_where_arr( stringarr, ['how', 'today'], count )
;                 2           6
;      IDL> print, count
;           2
;      IDL> print, wc_where_arr( stringarr, ['how', 'today'], /notequal )
;                 0       1       3       4       5       7
;      IDL> print, wc_where_arr( stringarr, ['how', 'TODAY'] )
;                 2
;      IDL> print, wc_where_arr( stringarr, ['how', 'TODAY'], /CASE_IGNORE )
;                 2           6
;
;   History:
;      acs 1999-dec  extension of slf's wc_where, which gets called for each pattern
;-


n_pattern = N_Elements( in_pattern_array )
n_in_array =  N_Elements( in_array )

FOR i=0, n_pattern-1 DO BEGIN 
    list = WC_Where( in_array, in_pattern_array[i], CASE_IGNORE=CASE_ignore )
    full_list = Append_Arr( full_list, list )
ENDFOR

full_list = Get_Uniq( full_list )

IF full_list[0] EQ -1 THEN BEGIN 
    IF  N_Elements( full_list ) GT 1 THEN BEGIN 
        full_list = full_list[1:*]
    ENDIF ELSE BEGIN 
        count = 0
        RETURN, -1
    ENDELSE
ENDIF

IF NOT Keyword_Set( NOTEQUAL ) THEN BEGIN 
    count = N_Elements( full_list )
    RETURN, full_list
ENDIF ELSE BEGIN
    RETURN, Where_Arr( Lindgen( n_in_array ), full_list, count, /NOTEQUAL )
ENDELSE

END




 