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
;   Joseph Plowman
;-
pro comp_fix_quxtalk, date_dir, images, headers, filename
  compile_opt strictarr

  comp_inventory_header, headers, beams, groups, waves, pols, type, expose, $
                         cover, cal_pol, cal_ret

  ; static Crosstalk correction (new values from Steve Oct 2014)
  iqxtalk = -0.000581
  iuxtalk =  0.004841
  quxtalk =  0.0
  uqxtalk =  0.0

  nimg = n_elements(images[0, 0, *])
  for i = 0L, nimg - 1L do begin
    if (pols[i] eq 'Q') then begin
      stokesI = comp_get_component(images, headers, 'I', beams[i], waves[i], /noskip)
      stokesU = comp_get_component(images, headers, 'U', beams[i], waves[i], /noskip)
      images[*, *, i] -= iqxtalk * stokesI + uqxtalk * stokesU
    endif
    if (pols[i] eq 'U') then begin
      stokesQ = comp_get_component(images, headers, 'Q', beams[i], waves[i], /noskip)
      stokesI = comp_get_component(images, headers, 'I', beams[i], waves[i], /noskip)
      images[*, *, i] -= iuxtalk * stokesI + quxtalk * stokesQ
    endif
  endfor
end
