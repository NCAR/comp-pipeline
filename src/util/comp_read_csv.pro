; docformat = 'rst'

;+
; Wrapper for READ_CSV that works for CSV files with only a single line.
;
; :Params:
;   filename : in, required, type=string
;     filename of CSV file
;
; :Keywords:
;   types : in, optional, type=lonarr
;     `SIZE` types of output fields
;-
function comp_read_csv, filename, types=types
  compile_opt strictarr

  datatypes = ['', 'Byte', 'Int', 'Long', 'Float', 'Double', '', 'String', $
               '', '', '', '', 'Uint', 'Ulong', 'Long64', 'Ulong64']
  if (n_elements(types) gt 0L) then _types = datatypes[types]
  data = read_csv(filename, types=_types)
  if (n_elements(types) eq 0L) then return, data

  result = {}
  tnames = tag_names(data)
  for c = 0L, n_elements(types) - 1L do begin
    result = create_struct(result, tnames[c], fix(data.(c), type=types[c]))
  endfor

  return, result
end

