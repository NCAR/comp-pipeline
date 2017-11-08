; docformat = 'rst'
;+
; comp_fix_quxtalk
;
; Apply crosstalk correction to Q and/or U images.
; Currently just used a pair of static coefficients;
; Future versions may be more sophisticated. For this
; reason, has additional input parameters (the same
; set as for comp_fix_vxtalk).
;
; :Uses:
;   comp_inventory_header, comp_get_component
;
; :Params:
;   date_dir : in, required, type=string
;     the date directory for the Stokes Q or U images being corrected
;   images : in, required, type = real array (nx*ny*nimg)
;     the Stokes Q/U images to correct. On output, will contain the corrected
;     images
;   headers : in, required, type="strarr(ntags, nimg)"
;     the FITS headers corresponding to images
;   filename : in, required, type=string
;     the name of the file from which the Stokes Q/U images were read
;
; :Author:
;   MLSO Software Team
;-
pro comp_fix_quxtalk, date_dir, images, headers, filename
  compile_opt strictarr
  @comp_constants_common

  comp_inventory_header, headers, beams, waves, pols, type, expose, $
                         cover, cal_pol, cal_ret

  nimg = n_elements(images[0, 0, *])

  ; crosstalk coefficients are in epochs.cfg file
  for i = 0L, nimg - 1L do begin
    if (pols[i] eq 'Q') then begin
      stokesI = comp_get_component(images, headers, 'I', beams[i], waves[i], /noskip)
      stokesU = comp_get_component(images, headers, 'U', beams[i], waves[i], /noskip)
      images[*, *, i] -= i_to_q_xtalk * stokesI + u_to_q_xtalk * stokesU
    endif
    if (pols[i] eq 'U') then begin
      stokesQ = comp_get_component(images, headers, 'Q', beams[i], waves[i], /noskip)
      stokesI = comp_get_component(images, headers, 'I', beams[i], waves[i], /noskip)
      images[*, *, i] -= i_to_u_xtalk * stokesI + q_to_u_xtalk * stokesQ
    endif
  endfor
end
