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
    '1074': rest = double(center1074)
    '1079': rest = double(center1079)
    '1083': rest = double(center1083)
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
      vpix = (d_lambda / rest) * c    ; km/s/pix
      nx = (size(i1))[1]
      ny = (size(i1))[2]
      nz = (size(i1))[3] + 3
    endif

    if (qu_files[ii] eq 1) then begin
      stks_q = 0.5D * double(readfits(gbu[ii].l1file, $
                                      exten_no=n_points[ii] + wave_ind[0] + 1, $
                                      /silent) $
                               + readfits(gbu[ii].l1file, $
                                          exten_no=n_points[ii] + wave_ind[1] + 1, $
                                          /silent) $
                               + readfits(gbu[ii].l1file, $
                                          exten_no=n_points[ii] + wave_ind[2] + 1, $
                                          /silent))
      stks_u = 0.5D * double(readfits(gbu[ii].l1file, $
                                      exten_no=2 * n_points[ii] + wave_ind[0] + 1, $
                                      /silent) $
                               + readfits(gbu[ii].l1file, $
                                          exten_no=2 * n_points[ii] + wave_ind[1] + 1, $
                                          /silent) $
                               + readfits(gbu[ii].l1file, $
                                          exten_no=2 * n_points[ii] + wave_ind[2] + 1, $
                                          /silent))
    endif

    if (ii gt 0) then index = merge_struct(index, fitshead2struct(hdr))
    temp_data = dblarr(nx, ny, 3)
    mask = comp_l2_mask(hdr)
    bad_pixels_mask = bytarr(nx, ny)

    for xx = 0L, nx - 1L do begin
      for yy = 0L, ny - 1L do begin
        if (mask[xx, yy] eq 1) then begin
          ; compute analytical gaussfit
          profile = double([reform(i1[xx, yy]), $
                            reform(i2[xx, yy]), $
                            reform(i3[xx, yy])])
          sub_bad = where(profile le 0, n_bad)
          if (n_bad gt 0L) then profile[sub_bad] = 0.005D

          if (profile[1] gt int_min_thresh $
              && profile[1] lt int_max_thresh $
              && profile[0] gt 0.01 $
              && profile[2] gt 0.01 $
              && profile[0] lt int_max_thresh $
              && profile[1] lt int_max_thresh) then begin
            comp_analytic_gauss_fit, profile, d_lambda, doppler_shift, width, i_cent
          endif else begin
            bad_pixels_mask[xx, yy] = 1B
            i_cent        = 0D
            doppler_shift = 0D
            width         = 0D
          endelse

          if (abs(i_cent - profile[1]) gt 1.5 * profile[1]) then begin
            bad_pixels_mask[xx, yy] = 1B
            temp_data[xx, yy, 0] = 0D
            temp_data[xx, yy, 1] = 0D
            temp_data[xx, yy, 2] = 0D
          endif else begin
            temp_data[xx, yy, 0] = i_cent
            temp_data[xx, yy, 1] = rest + doppler_shift
            temp_data[xx, yy, 2] = width / sqrt(2.)
          endelse
        endif
      endfor
    endfor

    ; create enhance intensity image,
    ; convert line width to [km/s], mask data
    temp_int = reform(temp_data[*, *, 0])
    temp_int[where(mask eq 0)] = 0D
    int_prep = i2
    int_prep[where(mask eq 0)] = 0D
    int_enh = comp_intensity_enhancement(int_prep, headfits(gbu[ii].l1file))
    int_enh[where(mask eq 0)] = 0D
    thresh_unmasked = where(mask eq 1 $
                              and temp_int gt int_min_thresh $
                              and temp_int lt int_max_thresh, $
                            complement=thresh_masked)
    temp_velo = temp_data[*, *, 1]
    temp_velo[thresh_masked] = 0D

    pre_corr = dblarr(nx, ny, 2)
    pre_corr[*, *, 0] = temp_int
    pre_corr[*, *, 1] = temp_velo
    comp_doppler_correction, pre_corr, post_corr, wave_type, ewtrend, temptrend
    if (abs(temptrend) gt 0.01) then begin
      mg_log, 'potential bad doppler correction: temptrend = %f', temptrend, $
              name='comp', /warn
    endif
    temp_corr_velo = reform(post_corr[*, *, 1])
    temp_line_width = sqrt(2.) * (reform(temp_data[*, *, 2]) / d_lambda) * vpix   ; km/s

    bad_pixels = where(bad_pixels_mask, n_bad_pixels)
    if (n_bad_pixels gt 0L) then begin
      temp_velo[bad_pixels] = 0.0D
      temp_corr_velo[bad_pixels] = 0.0D
      temp_line_width[bad_pixels] = 0.0D
    endif

    ; convert temp_velo from wavelength to velocity
    temp_velo = (temp_velo - rest) * c / rest

    temp_int[thresh_masked]        = 0D
    temp_corr_velo[thresh_masked]  = 0D
    temp_line_width[thresh_masked] = 0D
    int_enh[thresh_masked]         = 0.0D

    if (qu_files[ii] eq 1) then begin
      stks_q[thresh_masked] = 0D
      stks_u[thresh_masked] = 0D
    endif

    ; find median of non-CME finite dop -> "real rest wavelength"
    good_dop_ind = where(finite(temp_velo) and abs(temp_velo) lt 80.0, n_good_dop)
    if (n_good_dop gt 0L) then begin
      median_rest_wavelength = median(temp_velo[good_dop_ind])
      temp_corr_velo = temp_velo - median_rest_wavelength
    endif else begin
      median_rest_wavelength = !values.f_nan
      temp_corr_velo = temp_velo
    endelse

    ;=== write out FITS files ===
    mg_log, 'write out FITS %d/%d @ %s', ii + 1, nt, wave_type, name='comp', /info

    ;=== dynamics package ===
    primary_header = comp_convert_header(headfits(gbu[ii].l1file))
    outfilename = filepath(strmid(file_basename(gbu[ii].l1file), 0, 26) $
                             + 'dynamics.fts', $
                           root=l2_process_dir)
    writefits, outfilename, blank, primary_header

    ; intensity
    extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                    exten=wave_ind[1] + 1), $
                                           /exten, $
                                           extname='Intensity', $
                                           datminmax=[min(temp_int), $
                                                      max(temp_int)])
    sxdelpar, extension_header, 'SIMPLE'
    writefits, outfilename, float(temp_int), extension_header, /append

    ; enhanced intensity
    extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                    exten=wave_ind[1] + 1), $
                                           /exten, $
                                           extname='Enhanced Intensity', $
                                           datminmax=long([min(int_enh), $
                                                           max(int_enh)]))
    sxdelpar, extension_header, 'SIMPLE'
    sxaddpar, extension_header, 'BITPIX', 8
    writefits, outfilename, int_enh, extension_header, /append

    ; corrected LOS velocity
    extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                    exten=wave_ind[1] + 1), $
                                           /exten, $
                                           extname='Corrected LOS velocity', $
                                           datminmax=[min(temp_corr_velo), $
                                                      max(temp_corr_velo)])
    fxaddpar, extension_header, 'RESTWVL', median_rest_wavelength, $
              ' [km/s] rest wavelength', format='(F0.3)', /null
    sxdelpar, extension_header, 'SIMPLE'
    writefits, outfilename, float(temp_corr_velo), extension_header, /append
    sxdelpar, extension_header, 'RESTWVL'

    ; line width
    extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                    exten=wave_ind[1] + 1), $
                                           /exten, $
                                           extname='Line Width', $
                                           datminmax=[min(temp_line_width), $
                                                      max(temp_line_width)])
    sxdelpar, extension_header, 'SIMPLE'
    writefits, outfilename, float(temp_line_width), extension_header, /append

    if (add_uncorrected_velocity) then begin
      ; uncorrected LOS velocity
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                      exten=wave_ind[1] + 1), $
                                             /exten, $
                                             extname='Uncorrected LOS velocity', $
                                             datminmax=[min(temp_velo), $
                                                        max(temp_velo)])
      ;sxaddpar, extension_header, 'RESTWVL', rest, ' [km/s] rest wavelength', format='(F0.3)', /null
      sxdelpar, extension_header, 'SIMPLE'
      writefits, outfilename, float(temp_velo), extension_header, /append
      ;sxdelpar, extension_header, 'RESTWVL'
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
      primary_header = comp_convert_header(headfits(gbu[ii].l1file))
      outfilename = strmid(file_basename(gbu[ii].l1file), 0, 26) $
                      + 'polarization.fts'
      writefits, outfilename, blank, primary_header

      ; intensity
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                      exten=wave_ind[1] + 1), $
                                             /exten, $
                                             extname='Intensity', $
                                             datminmax=[min(temp_int), $
                                                        max(temp_int)])
      sxdelpar, extension_header, 'SIMPLE'
      writefits, outfilename, float(temp_int), extension_header, /append

      ; enhanced intensity
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                      exten=wave_ind[1] + 1), $
                                             /exten, $
                                             extname='Enhanced Intensity', $
                                             datminmax=long([min(int_enh), $
                                                             max(int_enh)]))
      sxdelpar, extension_header, 'SIMPLE'
      sxaddpar, extension_header, 'BITPIX', 8
      writefits, outfilename, int_enh, extension_header, /append

      ; Stokes Q
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                      exten=wave_ind[1] + 1), $
                                             /exten,$
                                             extname='Integrated Stokes Q', $
                                             datminmax=[min(stks_q), $
                                                        max(stks_q)])
      sxdelpar, extension_header, 'SIMPLE'
      writefits, outfilename, float(stks_q), extension_header, /append

      ; Stokes U
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                      exten=wave_ind[1] + 1), $
                                             /exten, $
                                             extname='Integrated Stokes U', $
                                             datminmax=[min(stks_u), $
                                                        max(stks_u)])
      sxdelpar, extension_header, 'SIMPLE'
      writefits, outfilename, float(stks_u), extension_header, /append

      ; Linear Polarization
      lin_pol = sqrt((stks_q)^2. + (stks_u)^2.)
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, $
                                                      exten=wave_ind[1] + 1), $
                                             /exten,$
                                             extname='Total Linear Polarization', $
                                             datminmax=[min(lin_pol), $
                                                        max(lin_pol)])
      sxdelpar, extension_header, 'SIMPLE'
      writefits, outfilename, float(lin_pol), extension_header, /append

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
