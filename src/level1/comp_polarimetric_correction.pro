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
;-
pro comp_polarimetric_correction, images, headers, p_angle
  compile_opt strictarr

  mg_log, 'p_angle %0.3f degrees', p_angle, name='comp', /debug

  comp_inventory_header, headers, beam, wave, pol

  q_ind = where(pol eq 'Q', q_count)
  u_ind = where(pol eq 'U', u_count)
  qb_ind = where(pol eq 'BKGQ', qb_count)
  ub_ind = where(pol eq 'BKGU', ub_count)

  if (q_count ne u_count) then begin
    mg_log, 'mismatching number of Q and U: %d Q images and %d U images', $
            q_count, u_count, $
            name='comp', /error
    return
  endif

  if (qb_count ne ub_count) then begin
    mg_log, 'mismatching number of BKGQ and BKGU: %d BKGQ images and %d BKGU images', $
            qb_count, ub_count, $
            name='comp', /error
    return
  endif

  mg_log, '%d Q/U extensions', q_count, name='comp', /debug
  mg_log, '%d BKGQ/BKGU extensions', qb_count, name='comp', /debug

  for i = 0L, q_count - 1L do begin
    mg_log, 'combining %s (ext %d, %0.2f nm) and %s (ext %d, %0.2f nm)', $
            pol[q_ind[i]], q_ind[i] + 1, wave[q_ind[i]], $
            pol[u_ind[i]], u_ind[i] + 1, wave[u_ind[i]], $
            name='comp', /debug
    comp_polarimetric_transform, images[*, *, q_ind[i]], $
                                 images[*, *, u_ind[i]], $
                                 p_angle, $
                                 new_q=new_q, $
                                 new_u=new_u

    images[*, *, q_ind[i]] = new_q
    images[*, *, u_ind[i]] = new_u
  endfor

  for i = 0L, qb_count - 1L do begin
    mg_log, 'combining %s (ext %d, %0.2f nm) and %s (ext %d, %0.2f nm)', $
            pol[qb_ind[i]], qb_ind[i] + 1, wave[qb_ind[i]], $
            pol[ub_ind[i]], ub_ind[i] + 1, wave[ub_ind[i]], $
            name='comp', /debug
    comp_polarimetric_transform, images[*, *, qb_ind[i]], $
                                 images[*, *, ub_ind[i]], $
                                 p_angle, $
                                 new_q=new_qb, $
                                 new_u=new_ub

    images[*, *, qb_ind[i]] = new_qb
    images[*, *, ub_ind[i]] = new_ub
  endfor

end
