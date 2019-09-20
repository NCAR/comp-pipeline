; docformat = 'rst'

;+
; Retrieve a product type ID given a product type name. Always returns a product
; type ID, uses the 'unknown' product type if the given product type name is not
; found.
;
; :Returns:
;   long
;
; :Params:
;   producttype_name : in, required, type=string
;     name of a product type
;
; :Keywords:
;   database : in, required, type=object
;     database object
;   count : out, optional, type=long
;     number of product types found matching given name; if 0, returns 'unknown'
;     product type
;-
function comp_get_producttype_id, producttype_name, database=db, count=count
  compile_opt strictarr

  q = 'select count(producttype_id) from mlso_producttype where producttype=''%s'''
  count_result = db->query(q, producttype_name)
  count = count_result.count_producttype_id_

  _producttype_name = count eq 0 ? 'unk' : producttype_name

  results = db->query('select * from mlso_producttype where producttype=''%s''', $
                            _producttype_name, fields=fields)
  return, results[0].producttype_id
end
