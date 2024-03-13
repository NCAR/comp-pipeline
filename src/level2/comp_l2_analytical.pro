; docformat = 'rst'

;+
; Create dynamics and polarization L2 files.
;
; :Examples:
;   For example, call it like::
;
;     comp_l2_analytical, '20130514', '1074', 3
;
; :Uses:
;   comp_constants_common, comp_config_common, comp_read_gbu, comp_l2_mask,
;   comp_analytic_gauss_fit, comp_intensity_enhancement,
;   comp_doppler_correction, comp_convert_header,
;   sxpar, headfits, readfits, fitshead2struct, merge_struct, writefits,
;   sxaddpar, sxdelpar, mg_log
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :Keywords:
;   nwl : in, required, type=integer
;     number of wavelengths to use; must be 3 right now
;
; :Author:
;   MLSO Software Team
;
; :History:
;   Christian Bethge
;   removed gzip    Oct 1 2014  GdT
;   see git log for recent changes
;-
pro comp_l2_analytical, date_dir, wave_type, nwl=nwl
  compile_opt strictarr
  @comp_constants_common
  @comp_config_common

  mg_log, 'wave_type: %s', wave_type, name='comp', /info

  l1_process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)
  l2_process_dir = filepath('', subdir=[date_dir, 'level2'], root=process_basedir)
  cd, l2_process_dir

  date = date_dir
  wave = wave_type

  case wave_type of
    '1074': begin
        rest = double(center1074)
        nominal = double(nominal_1074)
        int_min_thresh = int_min_1074_thresh
      end
    '1079': begin
        rest = double(center1079)
        nominal = double(nominal_1074)
        int_min_thresh = int_min_1079_thresh
      end
    '1083': begin
        rest = double(center1083)
        nominal = double(center1083)
        int_min_thresh = int_min_1079_thresh
      end
  endcase
  c = 299792.458D

  gbu_file = filepath(string(date_dir, wave_type, $
                             format='(%"%s.comp.%s.gbu.log")'), $
                      root=l1_process_dir)
  if (~file_test(gbu_file)) then begin
    mg_log, '%s does not exist, skipping', file_basename(gbu_file), name='comp', /warning
    goto, skip
  endif
  gbu = comp_read_gbu(gbu_file, count=count)
  if (count eq 0) then begin
    mg_log, 'no entries in GBU file %s', file_basename(gbu_file), name='comp', /warning
    goto, skip
  endif

  for ii = 0L, n_elements(gbu) - 1L do begin
    gbu[ii].l1file = filepath(gbu[ii].l1file, root=l1_process_dir)
  endfor

  ; only want the good measurements with at least nwl wavelengths
  good = where(gbu.reason eq 0 and gbu.wavelengths ge nwl, n_good)
  mg_log, '%d good %d point files...', n_good, nwl, name='comp', /info

  if (n_good eq 0) then goto, skip
  gbu = gbu[good]
  n_points = gbu.wavelengths

  qu_files = intarr(n_elements(gbu))
  nt = n_elements(gbu)

  ; distinguish between Q/U files and V files
  for ii = 0L, nt - 1L do begin
    l1_header = headfits(gbu[ii].l1file, exten=n_points[ii] + 1)
    l1_extname = sxpar(l1_header, 'EXTNAME')
    whatisthis = strmid(l1_extname, 0, 1)
    if (whatisthis eq 'Q') then qu_files[ii] = 1
  endfor

  for ii = 0L, nt - 1L do begin
    basename = file_basename(gbu[ii].l1file)
    fits_open, gbu[ii].l1file, fcb
    comp_inventory, fcb, beam, wave, error=error
    if (error gt 0L) then begin
      mg_log, 'skipping %s', gbu[ii].l1file, name='comp', /error
      continue
    endif
    fits_close, fcb

    hdr    = headfits(gbu[ii].l1file)

    wave_ind = comp_3pt_indices(wave_type, wave, error=error)

    i1 = double(readfits(gbu[ii].l1file, exten_no=wave_ind[0] + 1, /silent, ehdr1))
    i2 = double(readfits(gbu[ii].l1file, exten_no=wave_ind[1] + 1, /silent, ehdr2))
    i3 = double(readfits(gbu[ii].l1file, exten_no=wave_ind[2] + 1, /silent, ehdr3))

    if (ii eq 0) then begin
      wavel = [sxpar(ehdr1, 'WAVELENG'), $
               sxpar(ehdr2, 'WAVELENG'), $
               sxpar(ehdr3, 'WAVELENG')]
      index = fitshead2struct(hdr)
      d_lambda = double(mean(deriv(wavel)))
      nx = (size(i1))[1]
      ny = (size(i1))[2]
      nz = (size(i1))[3] + 3
    endif

    if (qu_files[ii] eq 1) then begin
      stks_q = 0.3D * double(readfits(gbu[ii].l1file, $
                                      exten_no=n_points[ii] + wave_ind[0] + 1, $
                                      /silent)) $
                 + 0.7D * double(readfits(gbu[ii].l1file, $
                                          exten_no=n_points[ii] + wave_ind[1] + 1, $
                                          /silent)) $
                 + 0.3D * double(readfits(gbu[ii].l1file, $
                                          exten_no=n_points[ii] + wave_ind[2] + 1, $
                                          /silent))
      stks_u = 0.3D * double(readfits(gbu[ii].l1file, $
                                      exten_no=2 * n_points[ii] + wave_ind[0] + 1, $
                                      /silent)) $
                 + 0.7D * double(readfits(gbu[ii].l1file, $
                                          exten_no=2 * n_points[ii] + wave_ind[1] + 1, $
                                          /silent)) $
                 + 0.3D * double(readfits(gbu[ii].l1file, $
                                          exten_no=2 * n_points[ii] + wave_ind[2] + 1, $
                                          /silent))
    endif

    if (ii gt 0) then index = merge_struct(index, fitshead2struct(hdr))
    temp_data = dblarr(nx, ny, 3)

    mask = comp_l2_mask(hdr)
    bad_pixels_mask = bytarr(nx, ny)

    for xx = 0L, nx - 1L do begin
      for yy = 0L, ny - 1L do begin
        IF (mask[xx, yy] eq 1) THEN BEGIN
          ; compute analytical gaussfit
          profile = double([reform(i1[xx, yy]), $
                            reform(i2[xx, yy]), $
                            reform(i3[xx, yy])])
         if min(profile) gt 0 then begin
            comp_analytic_gauss_fit, profile, d_lambda, doppler_shift, width, i_cent
            temp_data[xx, yy, 0] = i_cent
            temp_data[xx, yy, 1] = doppler_shift  
            temp_data[xx, yy, 2] = width 
            ; if gaussian fits cannot be performed update mask of bad pixels
            if doppler_shift eq 0 and width eq 0 and i_cent eq 0 then bad_pixels_mask[xx, yy] = 1B   ; bad_pixels_mask has 1 where gausssian fit could not be done
         endif else begin
            bad_pixels_mask[xx, yy] = 1B    ; bad_pixels_mask has 1 where pixels have negative intensity
            temp_data[xx, yy, 0] = 0.0D
            temp_data[xx, yy, 1] = 0.0D
            temp_data[xx, yy, 2] = 0.0D
         endelse
      ENDIF ELSE BEGIN
            bad_pixels_mask[xx, yy] = 1B    ; bad_pixels_mask has 1 where mask has zero
            temp_data[xx, yy, 0] = 0.0D
            temp_data[xx, yy, 1] = 0.0D
            temp_data[xx, yy, 2] = 0.0D
      ENDELSE    
      endfor
    endfor

    ; define peak intensity and enhanced intensity image and apply minimal masking 
    peak_int = reform(temp_data[*, *, 0])
    peak_int[where(bad_pixels_mask eq 1)] = 0D     ; exclude points where gaussian fit could not be performed

    enh_int = comp_intensity_enhancement(i2, headfits(gbu[ii].l1file))
    enh_int[where(mask eq 0)] = 0D                           ;enhanced intensity  has only geometric masking

     ;convert e-folding line width to FWHM and km/s   
    line_width = (reform(temp_data[*, *, 2])) *c / nominal 
    line_width_fwhm = line_width *fwhm_factor  
    line_width_fwhm[where(bad_pixels_mask eq 1)] = 0D ; exclude points where gaussian fit could not be performed
   
    ; velocity in km/s
    velo = temp_data[*, *, 1] + rest                      ; "rest" here is the center wavelength
    velo =  velo * c / nominal    
    velo[where(bad_pixels_mask eq 1)] = 0D ; exclude points where gaussian fit could not be performed

      
    ;***remove this part about the correction for rotation - is not used anymore***
    ;pre_corr = dblarr(nx, ny, 2)
    ;pre_corr[*, *, 0] = temp_int
    ;pre_corr[*, *, 1] = temp_velo
    ;comp_doppler_correction, pre_corr, post_corr, wave_type, ewtrend, temptrend
    ;if (abs(temptrend) gt 0.01) then begin
    ;  mg_log, 'potential bad doppler correction: temptrend = %f', temptrend, $
    ;          name='comp', /warn
    ;endif
    ;temp_corr_velo = reform(post_corr[*, *, 1])
    ;temp_corr_velo(bad_pixels_mask eq 1)] = 0D ;


; rest wavelength calculation
; added option to retun east and west values    
    rest_wavelength = comp_compute_rest_wavelength(hdr, $
                                                   velo, $
                                                   [[[i1]], [[i2]], [[i3]]], $
                                                   line_width_fwhm, $
                                                   method='median', $
                                                   indices=vel_indices, $
                                                   med_east=med_vel_east, med_west =med_vel_west)

 
    IF (n_elements(vel_indices) gt 0L and finite(rest_wavelength) ne 0 ) THEN BEGIN 
    ;case in which a rest wavelength was determined 
     corr_velo[vel_indices] = velo[vel_indices] - rest_wavelength

     ;define masking for velocity and line widh data - less restrictive than for movies and images
        good_vel_indices = where(mask eq 1 $
                              and bad_pixel_mask eq 0 $ ; exclude points where gaussian fit could not be performed
                              and abs(corr_velo) lt 100 $
                              and i1 gt 0.1 $
                              and i2 gt int_min_thresh $
                              and i3 gt 0.1 $
                              and i1 lt int_max_thresh $
                              and i2 lt int_max_thresh $
                              and i3 lt int_max_thresh $
                              and line_width_fwhm gt 36 $ 
                              and line_width_fwhm lt 170.0, $
                              ngood, $
                              ncomplement=n_bad_vel_pixels, $
                              complement=bad_vel_indices, $
                             /null)
;apply masking to velocity and line width
         if (n_bad_vel_pixels gt 0L) then begin
            velo[bad_vel_indices] = 0.0D
            corr_velo[bad_vel_indices] = 0.0D
            line_width_fwhm[bad_vel_indices] = 0.0D
         endif
    ENDIF ELSE BEGIN
;TODO    
 ; Mike: please add a warning if this happens, so we trace these bad frames where the rest wavelength could not be computed         
       peak_int[*]=0.0D
       velo[*] = 0.0D
       corr_velo[*] = 0.0D
       line_width_fwhm[*] = 0.0D 
    ENDELSE  
   
;   
; POLARIZATION FILE

   IF (qu_files[ii] eq 1) THEN BEGIN
      
;linear polarizattion
        lin_pol = sqrt((stks_q)^2. + (stks_u)^2.)
;azimuth
       azimuth = comp_azimuth(stks_u, stks_q, radial_azimuth=radial_azimuth)      
; average intensity for polarization file 
       averaged_intensity = 0.3 * i1 + 0.7 * i2 + 0.3 * i3

;apply mask less restrictive than for velocity and line with 
    good_pol_indices = where(mask gt 0 $
                               and i1 gt 0.05 $
                               and i2 gt 0.25 $
                               and i3 gt 0.05 $
                               and i1 lt int_max_thresh $
                               and i2 lt int_max_thresh $
                               and i3 lt int_max_thresh , $
                             ncomplement=n_bad_pol_pixels, $
                             complement=bad_pol_indices, /null)
;apply masking to polarization quantities
   if (n_bad_pol_pixels gt 0L) then begin   
      averaged_intensity[bad_pol_index] = 0.0D
      stks_q[bad_pol_indices] = 0.0D
      stks_u[bad_pol_indices] = 0.0D
      lin_pol[bad_pol_indices] = 0.0D
      azimuth[bad_pol_indices] = 0.0D
      radial_azimuth[bad_pol_indices] = 0.0D
endif

   ENDIF
 

;=== WRITE OUT  FITS FILES ===
    
    mg_log, '%d/%d @ %s: dynamics FITS', ii + 1, nt, wave_type, $
            name='comp', /info

    ;=== dynamics package ===
    primary_header = comp_convert_header(headfits(gbu[ii].l1file))
    outfilename = filepath(strmid(basename, 0, 26) $
                             + 'dynamics.fts', $
                           root=l2_process_dir)
    writefits, outfilename, blank, primary_header

    ; intensity
    extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                    exten=wave_ind[1] + 1), $
                                           /exten, $
                                           extname='Peak intensity', $
                                           datminmax=[min(peak_int), $
                                                      max(peak_int)])
    sxdelpar, extension_header, 'SIMPLE'
    writefits, outfilename, float(peak_int), extension_header, /append

    ; enhanced intensity
    extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                    exten=wave_ind[1] + 1), $
                                           /exten, $
                                           extname='Enhanced Intensity', $
                                           datminmax=long([min(enh_int), $
                                                           max(ehn_int)]))
    sxdelpar, extension_header, 'SIMPLE'
    sxaddpar, extension_header, 'BITPIX', 8
    writefits, outfilename, temp_enh_int, extension_header, /append

    ; corrected LOS velocity
    extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                    exten=wave_ind[1] + 1), $
                                           /exten, $
                                           extname='Corrected LOS velocity', $
                                           datminmax=[min(corr_velo), $
                                                      max(corr_velo)])
    fxaddpar, extension_header, 'RSTWVL', rest_wavelength, $
              ' [km/s] rest wavelength', format='(F0.3)', /null

    sxdelpar, extension_header, 'SIMPLE'
    writefits, outfilename, float(corr_velo), extension_header, /append
    sxdelpar, extension_header, 'RSTWVL'

    ; line width
    
    extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                    exten=wave_ind[1] + 1), $
                                           /exten, $
                                           extname='Line width (FWHM)', $
                                           datminmax=[min(line_width_fwhm), $
                                                      max(line_width_fwhm)])
    sxdelpar, extension_header, 'SIMPLE'
    writefits, outfilename, float(line_width_fwhm), extension_header, /append

    ; center wavelength intensity
    extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                    exten=wave_ind[1] + 1), $
                                           /exten, $
                                           extname='Center wavelength intensity', $
                                           datminmax=[min(i2), $
                                                      max(i2)])
    sxdelpar, extension_header, 'SIMPLE'
    writefits, outfilename, float(i2), extension_header, /append

    if (add_uncorrected_velocity) then begin
      ; uncorrected LOS velocity
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                      exten=wave_ind[1] + 1), $
                                             /exten, $
                                             extname='Uncorrected LOS velocity', $
                                             datminmax=[min(velo), $
                                                        max(velo)])
      sxdelpar, extension_header, 'SIMPLE'
      writefits, outfilename, float(velo), extension_header, /append
    endif

    zip_cmd = string(outfilename, format='(%"gzip -f %s")')
    spawn, zip_cmd, result, error_result, exit_status=status
    if (status ne 0L) then begin
      mg_log, 'problem zipping dynamics file with command: %s', zip_cmd, $
              name='comp', /error
      mg_log, '%s', error_result, name='comp', /error
    endif

    ;=== polarization package ===
    if (qu_files[ii] eq 1) then begin
      mg_log, '%d/%d @ %s: polarization FITS', ii + 1, nt, wave_type, $
              name='comp', /info

      primary_header = comp_convert_header(headfits(gbu[ii].l1file))
      outfilename = strmid(basename, 0, 26) + 'polarization.fts'
      writefits, outfilename, blank, primary_header

      ; intensity
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                      exten=wave_ind[1] + 1), $
                                             /exten, $
                                             extname='Weighted average intensity', $
                                             datminmax=[min(averaged_intensity), $
                                                        max(averaged_intensity)])
      sxdelpar, extension_header, 'SIMPLE'
      writefits, outfilename, float(averaged_intensity), extension_header, /append

      ; enhanced intensity
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                      exten=wave_ind[1] + 1), $
                                             /exten, $
                                             extname='Enhanced Intensity', $
                                             datminmax=long([min(int_enh), $
                                                             max(int_enh)]))
      sxdelpar, extension_header, 'SIMPLE'
      sxaddpar, extension_header, 'BITPIX', 8
      writefits, outfilename, temp_enh_int, extension_header, /append

      ; Stokes Q
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                      exten=wave_ind[1] + 1), $
                                             /exten,$
                                             extname='Weighted average Q', $
                                             datminmax=[min(stks_q), $
                                                        max(stks_q)])
      sxdelpar, extension_header, 'SIMPLE'
      writefits, outfilename, float(stks_q), extension_header, /append

      ; Stokes U
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                      exten=wave_ind[1] + 1), $
                                             /exten, $
                                             extname='Weighted average U', $
                                             datminmax=[min(stks_u), $
                                                        max(stks_u)])
      sxdelpar, extension_header, 'SIMPLE'
      writefits, outfilename, float(stks_u), extension_header, /append

      ; Linear Polarization
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                      exten=wave_ind[1] + 1), $
                                             /exten,$
                                             extname='Weighted average L', $
                                             datminmax=[min(lin_pol), $
                                                        max(lin_pol)])
      sxdelpar, extension_header, 'SIMPLE'
      writefits, outfilename, float(lin_pol), extension_header, /append

      ; Azimuth
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                      exten=wave_ind[1] + 1), $
                                             /exten,$
                                             extname='Azimuth', $
                                             datminmax=[min(azimuth), $
                                                        max(azimuth)])
      sxdelpar, extension_header, 'SIMPLE'
      writefits, outfilename, float(azimuth), extension_header, /append

      ; Radial azimuth
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                      exten=wave_ind[1] + 1), $
                                             /exten,$
                                             extname='Radial azimuth', $
                                             datminmax=[min(radial_azimuth), $
                                                        max(radial_azimuth)])
      sxdelpar, extension_header, 'SIMPLE'
      writefits, outfilename, float(radial_azimuth), extension_header, /append

      ; center wavelength intensity
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                      exten=wave_ind[1] + 1), $
                                             /exten, $
                                             extname='Center wavelength intensity', $
                                             datminmax=[min(i2), $
                                                        max(i2)])
      sxdelpar, extension_header, 'SIMPLE'
      writefits, outfilename, float(i2), extension_header, /append

      zip_cmd = string(outfilename, format='(%"gzip -f %s")')
      spawn, zip_cmd, result, error_result, exit_status=status
      if (status ne 0L) then begin
        mg_log, 'problem zipping polarization file with command: %s', zip_cmd, $
                name='comp', /error
        mg_log, '%s', error_result, name='comp', /error
      endif
    endif
  endfor

  skip:
  mg_log, 'done', name='comp', /info
end


; main-level example program

;dates = ['20120517']
dates = ['20110103', '20110309', '20110521', '20110701', '20110904', $
         '20111107', '20110203', '20110401', '20110622', '20110801', $
         '20111016', '20111201']
wave_types = ['1074', '1079']

;config_basename = 'comp.intermediate.cfg'
config_basename = 'comp.reprocess-check-2011.cfg'
config_filename = filepath(config_basename, $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())
comp_configuration, config_filename=config_filename

for d = 0L, n_elements(dates) - 1L do begin
  comp_update_configuration, dates[d]
  comp_initialize, dates[d]

  for w = 0L, n_elements(wave_types) - 1L do begin
    comp_l2_analytical, dates[d], wave_types[w], nwl=3
  endfor
endfor

end
