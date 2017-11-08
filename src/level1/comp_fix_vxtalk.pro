; docformat = 'rst'

;+
; Fix the crosstalk in a set of CoMP Stokes V images. Will find and load the
; nearest (in time) set of Stokes Q and U images to use to estimate the
; crosstalk. The crosstalk is estimated by assuming the CoMP continuum Stokes V
; is dominated by crosstalk and fit using the routine comp_find_vxtalk; see
; that routine for more details of the crosstalk estimation process.
;
; :Uses:
;   comp_inventory_header, comp_nearest_qufile, comp_read_data,
;   comp_apply_flats_darks, comp_demodulate, comp_get_component, comp_raw_mask,
;   comp_find_vxtalk
;
; :Params:
;   date_dir : in, required, type=string
;     the date directory for the Stokes V images being corrected; used to find
;     the nearest Q and U files and to find image masks
;   vimages : in, required, type="fltarr(nx, ny, nimg)"
;     the Stokes V images to correct; on output, will contain the corrected
;     images
;   vheaders : in, required, type="strarr(ntags, nimg)"
;     the FITS headers corresponding to vimages
;   filename : in, required, type=string
;     the name of the file from which the Stokes V images were read; necessary
;     to find the nearest Q and U file
;
; :Author:
;   MLSO Software Team
;-
pro comp_fix_vxtalk, date_dir, vimages, vheaders, filename
  compile_opt strictarr
  @comp_constants_common

  comp_inventory_header, vheaders, beams, waves, pols, type, expose, $
                         cover, cal_pol, cal_ret

  ; find the nearest Q and U file and prepare it for crosstalk estimation
  qufile = comp_nearest_qufile(date_dir, vheaders, filename)
  comp_read_data, qufile, quimages, quheaders, quheader0
  comp_apply_flats_darks, quimages, quheaders, date_dir
  comp_demodulate, quimages, quheaders, quimages_demod, quheaders_demod

  ; apply the estimated crosstalk correction to vimages (both on-band and
  ; continuum)
  nimg = n_elements(vimages[0, 0, *])
  for i = 0L, nimg - 1L do begin
    if (pols[i] eq 'V') then begin
      stokesI = comp_get_component(quimages_demod, quheaders_demod, 'I', $
                                   beams[i], waves[i], /noskip)
      stokesQ = comp_get_component(quimages_demod, quheaders_demod, 'Q', $
                                   beams[i], waves[i], /noskip)
      stokesU = comp_get_component(quimages_demod, quheaders_demod, 'U', $
                                   beams[i], waves[i], /noskip)
      vimages[*, *, i] -= i_to_v_xtalk * stokesI $
                            + q_to_v_xtalk * stokesQ $
                            + u_to_v_xtalk * stokesU
    endif
  endfor
end
