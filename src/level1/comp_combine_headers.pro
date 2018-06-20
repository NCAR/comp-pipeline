; docformat = 'rst'

;+
; Given multiple headers, combine them into a single header.
;
; Most header values should be the same for the combined headers, i.e.,
; polstate, beam, wavelength, etc., but some values need to be combined, e.g.,
; the RAWEXT keyword values.
; 
; :Returns:
;   a single FITS header, i.e., `strarr(n)`
;
; :Params:
;   headers : in, required, type="strarr(ntags, nimages)"
;     all headers
;   indices : in, required, type=lonarr
;     indices into dimension 2 of `headers` to combine
;-
function comp_combine_headers, headers, indices
  compile_opt strictarr

  n = n_elements(indices)
  combined_header = reform(headers[*, indices[0]])

  ; concatenate RAWEXT keyword values
  rawext = strarr(n)
  for i = 0L, n - 1L do rawext[i] = sxpar(headers[*, indices[i]], 'RAWEXT')
  sxaddpar, combined_header, 'RAWEXT', strjoin(rawext, ',')

  return, combined_header
end
