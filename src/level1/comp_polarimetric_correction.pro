; docformat = 'rst'

;+
; Correct images with heliographic coordinate transformations on Q and U.
;
; :Params:
;   images : in, out, required, type="fltarr(nx, ny, n_images)"
;     an array of images
;   headers : in, required, type="strarr(n_tags, n_images)"
;     an array of FITS extension headers, one for each image in the array
;   p_angle : in, required, type=float
;     p-angle
;-
pro comp_polarimetric_correction, images, headers, p_angle
  compile_opt strictarr

  comp_inventory_header, headers, beam, wave, pol

  q_ind = where(pol eq 'Q', q_count)
  u_ind = where(pol eq 'U', u_count)

  if (q_count ne u_count) then begin
    mg_log, 'mismatching number of Q and U: %d Q images and %d U images', $
            q_count, u_count, $
            name='comp', /error
    return
  endif

  for i = 0L, q_count - 1L do begin
    comp_polarimetric_transform, images[*, *, q_ind[i]], $
                                 images[*, *, u_ind[i]], $
                                 p_angle, $
                                 new_q=new_q, $
                                 new_u=new_u
    images[*, *, q_ind[i]] = new_q
    images[*, *, u_ind[i]] = new_u
  endfor
end
