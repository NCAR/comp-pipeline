; docformat = 'rst'

;+
; Retrieve a file type ID given a file type name. Always returns a file type ID,
; uses the 'unknown' file type if the given file type name is not found.
;
; :Returns:
;   long
;
; :Params:
;   filetype_name : in, required, type=string
;     name of a file type
;
; :Keywords:
;   database : in, required, type=object
;     database object
;   count : out, optional, type=long
;     number of file types found matching given name; if 0, returns 'unknown'
;     file type
;-
function comp_get_filetype_id, filetype_name, database=db, count=count
  compile_opt strictarr

  q = 'select count(filetype_id) from mlso_filetype where filetype=''%s'''
  count_result = db->query(q, filetype_name)
  count = count_result.count_filetype_id_

  _filetype_name = count eq 0 ? 'unk' : filetype_name

  results = db->query('select * from mlso_filetype where filetype=''%s''', $
                      _filetype_name, fields=fields)
  return, results[0].filetype_id
end
