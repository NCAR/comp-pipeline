; docformat = 'rst'


;+
; Find the appropriate upper left and lower right masks.
;
; :Params:
;   date_dir : in, required, type=string
;     the date directory for the Stokes V images being corrected; used to find
;     the nearest Q and U files and to find image masks
;   headers : in, required, type="strarr(ntags, nimg)"
;     the FITS headers corresponding to images
;
; :Keywords:
;   upper_left_mask : out, optional, type="bytarr(nx, ny)"
;     mask of the upper left portion of the image
;   lower_right_mask : out, optional, type="bytarr(nx, ny)"
;     mask of the lower right portion of the image
;-
pro comp_fix_vxtalk_getmasks, data_dir, headers, $
                              upper_left_mask=upper_left_mask, $
                              lower_right_mask=lower_right_mask
  compile_opt strictarr

  xmat = (dindgen(nx)) # transpose(1.0D + dblarr(ny)) - 0.5D * (nx - 1.0D)
  ymat = (1.0D + dblarr(nx)) # transpose(dindgen(ny)) - 0.5D * (ny - 1.0D)

  ; get the flat
  comp_inventory_header, headers, beam, group, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret
  time = comp_extract_time(headers, day, month, year, hours, mins, secs)
  comp_read_flats, date_dir, wave, beam, time, flat, flat_header, flat_waves, $
                   flat_names, flat_expose

  comp_make_mask, date_dir, flat_header, mask0 ; Compute the mask for this file.

  ; pad the file's mask using erode
  s = lonarr(5, 5) + 1L
  mask = erode(mask0, s)

  ; remove outlier pixels via median filtering.
  mask *= abs(datai / median(datai, 3) - 1.0) lt 0.25

  ; mask for pixels above the diagonal
  upper_left_mask = mask * (ymat gt xmat)
  ; mask for pixels below the diagonal
  lower_right_mask = mask * (xmat gt ymat)

  upper_left_mask[265:325, 400:600] = 0B
  lower_right_mask[680:740, 0:100] = 0B
end


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
;   Joseph Plowman
;-
pro comp_fix_vxtalk, date_dir, vimages, vheaders, filename
  compile_opt strictarr
  @comp_constants_common

  comp_inventory_header, vheaders, beams, groups, waves, pols, type, expose, $
                         cover, cal_pol, cal_ret

  ; find the nearest Q and U file and prepare it for crosstalk estimation
  qufile = comp_nearest_qufile(date_dir, vheaders, filename)
  comp_read_data, qufile, quimages, quheaders, quheader0
  comp_apply_flats_darks, quimages, quheaders, date_dir
  comp_demodulate, quimages, quheaders, quimages_demod, quheaders_demod

  ; break out the various Stokes pieces and average them over wavelength
  V1 = comp_get_component(vimages, vheaders, 'V', 1, $
                          /noskip, /average_wavelengths)
  V2 = comp_get_component(vimages, vheaders, 'V', -1, $
                          /noskip, /average_wavelengths)
  I1 = comp_get_component(quimages_demod, quheaders_demod, 'I', 1, $
                          /noskip, /average_wavelengths)
  I2 = comp_get_component(quimages_demod, quheaders_demod, 'I', -1, $
                          /noskip, /average_wavelengths)
  Q1 = comp_get_component(quimages_demod, quheaders_demod, 'Q', 1, $
                          /noskip, /average_wavelengths)
  Q2 = comp_get_component(quimages_demod, quheaders_demod, 'Q', -1, $
                          /noskip, /average_wavelengths)
  U1 = comp_get_component(quimages_demod, quheaders_demod, 'U', 1, $
                          /noskip, /average_wavelengths)
  U2 = comp_get_component(quimages_demod, quheaders_demod, 'U', -1, $
                          /noskip, /average_wavelengths)

  ; mask out the on-band beams and combine the continuum beams
  comp_fix_vxtalk_getmasks, date_dir, vheaders, $
                            upper_left_mask=mask1, lower_right_mask=mask2

  Ibg1 = I1 * mask1
  Ibg2 = I2 * mask2

  Qbg1 = Q1 * mask1
  Qbg2 = Q2 * mask2

  Ubg1 = U1 * mask1
  Ubg2 = U2 * mask2

  Vbg1 = V1 * mask1
  Vbg2 = V2 * mask2

  ; find the v crosstalk in the combined continuum beams
  comp_find_vxtalk, date_dir, Ibg1, Qbg1, Ubg1, Vbg1, vheaders, $
                    IVxtalk1, QVxtalk1, UVxtalk1, xtparm
  comp_find_vxtalk, date_dir, Ibg2, Qbg2, Ubg2, Vbg2, vheaders, $
                    IVxtalk2, QVxtalk2, UVxtalk2, xtparms

  IVxtalk = IVxtalk1 * mask1 + IVxtalk2 * mask2
  QVxtalk = QVxtalk1 * mask1 + QVxtalk2 * mask2
  UVxtalk = UVxtalk1 * mask1 + UVxtalk2 * mask2

  mg_log, '%s,%s', $
          file_basename(filename, '.FTS'), strjoin(strtrim(xtparms, 2), ','), $
          name='comp/crosstalk/' + comp_find_wavelength(waves, /name), /info

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
      vimages[*, *, i] -= IVxtalk * stokesI $
                            + QVxtalk * stokesQ $
                            + UVxtalk * stokesU
    endif
  endfor
end
