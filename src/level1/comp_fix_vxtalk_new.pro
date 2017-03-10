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
;   Joseph Plowman
;-
pro comp_fix_vxtalk, date_dir, vimages, vheaders, filename, beam_combined=beam_combined, wave_type=wave_type, header0=header0
  compile_opt strictarr
  @comp_constants_common
  @comp_config_common

  comp_inventory_header, vheaders, beams, waves, pols, type, expose, $
                         cover, cal_pol, cal_ret

  ; find the nearest Q and U file and prepare it for crosstalk estimation
  qufile = comp_nearest_qufile(date_dir, vheaders, filename)
  comp_read_data, qufile, quimages, quheaders, quheader0
  comp_apply_flats_darks, quimages, quheaders, date_dir
  comp_demodulate, quimages, quheaders, quimages_demod, quheaders_demod
  if(keyword_set(beam_combined)) then begin
	  ; split the foreground (on-band) and background (continuum) beams into
	  ; separate images, and subtract the backgrounds from the foregrounds. Store
	  ; each into its own set of images with updated headers.
	  comp_combine_beams, quimages_demod, quheaders_demod, date_dir, $
		              quimages_combine, quheaders_combine, quheader0, $
		              n_uniq_polstates=np, n_uniq_wavelengths=nw, $
		              image_geometry=image_geometry, $
		              wave_type=wave_type
	Vbg = comp_get_component(vimages, vheaders, 'BKGV', 0, /noskip, /average_wavelengths,headersout=vheaderavg)
	Ibg = comp_get_component(vimages, vheaders, 'BKGI', 0, /noskip, /average_wavelengths)
	Qbg = comp_get_component(quimages_combine, quheaders_combine, 'BKGQ', 0, /noskip, /average_wavelengths)
	Ubg = comp_get_component(quimages_combine, quheaders_combine, 'BKGU', 0, /noskip, /average_wavelengths)
        comp_make_mask, date_dir, header0, mask
        quimages_demod = quimages_combine
        quheaders_demod = quheaders_combine
  endif	else begin  
	; Break out the various Stokes pieces and average them over wavelength:
	V1 = comp_get_component(vimages, vheaders, 'V', 1, /noskip, /average_wavelengths)
	V2 = comp_get_component(vimages, vheaders, 'V', -1, /noskip, /average_wavelengths)
	I1 = comp_get_component(vimages, vheaders, 'I', 1, /noskip, /average_wavelengths)
	I2 = comp_get_component(vimages, vheaders, 'I', -1, /noskip, /average_wavelengths)
	Q1 = comp_get_component(quimages_demod, quheaders_demod, 'Q', 1, /noskip, /average_wavelengths)
	Q2 = comp_get_component(quimages_demod, quheaders_demod, 'Q', -1, /noskip, /average_wavelengths)
	U1 = comp_get_component(quimages_demod, quheaders_demod, 'U', 1, /noskip, /average_wavelengths)
	U2 = comp_get_component(quimages_demod, quheaders_demod, 'U', -1, /noskip, /average_wavelengths)

	; Mask out the on-band beams and combine the continuum beams:
	; To do: The call to comp_raw mask should be replaced with comp_make_mask_1024...
	mask=comp_raw_mask(date_dir,vheaders,upper_left_mask=mask1, lower_right_mask=mask2)
	Ibg = I1*mask1+I2*mask2	
	Qbg = Q1*mask1+Q2*mask2	
	Ubg = U1*mask1+U2*mask2	
	Vbg = V1*mask1+V2*mask2
  endelse

  ; estimate of shot noise
  photfac = double(1.0/875.0)
  vcerrs = sqrt(abs(Ibg) * photfac) * mask
  print,max(mask),min(mask)
  print,max(vcerrs),min(vcerrs)
  help,where(mask eq 0)
  help,where(mask eq 1)

  snrfac_mask = 6 ; Mask out pixels with SNR+1 more than this many MADs above the median.
  snrp1 = 1+abs(Vbg)/vcerrs
  snrp1_med = median(snrp1[where(mask)])
  snrp1_mad = median(abs(snrp1_med-snrp1[where(mask)])) ; The median absolute deviation (MAD).
  mask = mask*(snrp1 lt (snrp1_med+snrfac_mask*snrp1_mad))
  if (n_elements(erodenum) eq 0L) then erodenum = 6
  mask = erode(mask , replicate(1, erodenum, erodenum))

	; Find the v crosstalk in the combined continuum beams:
	comp_find_vxtalk,Ibg,Qbg,Ubg,Vbg, vcerrs, mask,IVxtalk,QVxtalk,UVxtalk,xtparms


  ; apply the estimated crosstalk correction to vimages (both on-band and
  ; continuum)
  nimg = n_elements(vimages[0, 0, *])
  for i = 0L, nimg - 1L do begin
    if (pols[i] eq 'V') then begin
      stokesI = comp_get_component(vimages, vheaders, 'I', $
                                   beams[i], waves[i], /noskip)
      stokesQ = comp_get_component(quimages_demod, quheaders_demod, 'Q', $
                                   beams[i], waves[i], /noskip)
      stokesU = comp_get_component(quimages_demod, quheaders_demod, 'U', $
                                   beams[i], waves[i], /noskip)
      vimages[*, *, i] -= IVxtalk * stokesI $
                            + QVxtalk * stokesQ $
                            + UVxtalk * stokesU
    endif
    if (pols[i] eq 'BKGV') then begin
      stokesI = comp_get_component(vimages, vheaders, 'BKGI', $
                                   beams[i], waves[i], /noskip)
      stokesQ = comp_get_component(quimages_demod, quheaders_demod, 'BKGQ', $
                                   beams[i], waves[i], /noskip)
      stokesU = comp_get_component(quimages_demod, quheaders_demod, 'BKGU', $
                                   beams[i], waves[i], /noskip)
      vimages[*, *, i] -= IVxtalk * stokesI $
                            + QVxtalk * stokesQ $
                            + UVxtalk * stokesU
    endif
  endfor
  
  ; Generate the output filename and plot to jpeg:
  calplot_dir = filepath('stokesv_crosstalk_plots',subdir=comp_decompose_date(date_dir),root_dir=engineering_dir)
  if(~file_test(calplot_dir)) then file_mkdir,calplot_dir
  calplot_file = filepath(date_dir,root_dir=calplot_dir)
  qub = file_basename(qufile)
  fnb = file_basename(filename)
  comp_plot_vxtalk, Ibg, Qbg, Ubg, Vbg, IVxtalk, QVxtalk, UVxtalk, mask, calplot_file, qub, qub, qub, fnb
  
end



