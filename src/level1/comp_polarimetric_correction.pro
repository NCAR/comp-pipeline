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
;     p-angle in degrees
;   overlap_angle, in, required, type=float
;     overlap angle in degrees
;-
pro comp_polarimetric_correction, images, headers, p_angle, overlap_angle
  compile_opt strictarr

  mg_log, 'p_angle %0.3f degrees, overlap_angle %0.3f degrees', $
          p_angle, overlap_angle, name='comp', /debug

  comp_inventory_header, headers, beam, wave, pol

  q_ind = where(pol eq 'Q', q_count)
  u_ind = where(pol eq 'U', u_count)

  if (q_count ne u_count) then begin
    mg_log, 'mismatching number of Q and U: %d Q images and %d U images', $
            q_count, u_count, $
            name='comp', /error
    return
  endif

  mg_log, '%d Q/U extensions', q_count, name='comp', /debug

  for i = 0L, q_count - 1L do begin
    mg_log, 'combining %s (ext %d, %0.2f nm) and %s (ext %d, %0.2f nm)', $
            pol[q_ind[i]], q_ind[i] + 1, wave[q_ind[i]], $
            pol[u_ind[i]], u_ind[i] + 1, wave[u_ind[i]], $
            name='comp', /debug
    comp_polarimetric_transform, images[*, *, q_ind[i]], $
                                 images[*, *, u_ind[i]], $
                                 p_angle, overlap_angle, $
                                 new_q=new_q, $
                                 new_u=new_u

    images[*, *, q_ind[i]] = new_q
    images[*, *, u_ind[i]] = new_u
  endfor
end
