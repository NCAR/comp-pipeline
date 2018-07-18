; docformat = 'rst'

;+
; Default routine to fix issues in raw data.
;
; :Params:
;   images : in, out, required, type="fltarr(nx, ny, n_extensions)"
;     the array of images read from the file, starting with extension 1
;   headers : out, required, type="strarr(n_tags, n_extensions)"
;     the FITS headers for each extension
;   primary_header : out, optional, type=strarr
;     the primary header (header for extension 0) from the file
;-
pro comp_data_default, images, headers, primary_header
  compile_opt strictarr

  ; don't need to fix anything by default
end
