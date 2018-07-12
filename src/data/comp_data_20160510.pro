; docformat = 'rst'

;+
; Routine to fix issues in raw data on 201605{10,11}.
;
; These days have NDFILTER 0, when it should be 8. This is for each 1074 and
; 1079 flat, dark, and data file before the first 1083 data file.
;
; :Params:
;   images : in, out, required, type="fltarr(nx, ny, n_extensions)"
;     the array of images read from the file, starting with extension 1
;   headers : out, required, type="strarr(n_tags, n_extensions)"
;     the FITS headers for each extension
;   primary_header : out, optional, type=strarr
;     the primary header (header for extension 0) from the file
;-
pro comp_data_20160510, images, headers, primary_header
  compile_opt strictarr

  wavelength = sxpar(reform(headers[*, 0]), 'WAVELENG')
  wave_type = comp_find_wave_type(wavelength, /name)

  ; only need to correct 1074 and 1079
  if (wave_type eq '1083') then return

  dims = size(headers, /dimensions)
  for e = 0L, dims[1] - 1L do begin
    h = reform(headers[*, e])
    nd_filter = sxpar(h, 'NDFILTER')
    if (nd_filter eq 0) then sxaddpar, h, 'NDFILTER', 8
    headers[*, e] = h
  endfor
end
