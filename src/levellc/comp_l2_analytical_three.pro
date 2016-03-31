; docformat = 'rst'

;+
; :Examples:
;   For example, call it like::
;
;     comp_l2_analytical_three, '20130514', '1074'
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :Author:
;   Christian Bethge
;
; :History:
;    removed gzip    Oct 1 2014  GdT
;-
pro comp_l2_analytical_three, date_dir, wave_type
  compile_opt strictarr
  @comp_constants_common
  @comp_paths_common

  ; configure
  comp_initialize, date_dir
  comp_paths, date_dir

  mg_log, 'wave_type: %s', wave_type, name='comp', /info

  process_dir = filepath(date_dir, root=process_basedir)
  cd, process_dir

  date = date_dir
  wave = wave_type
  nwl = 3

  case wave_type of
   '1074': rest = double(center1074)
   '1079': rest = double(center1079)
   '1083': rest = double(center1083)
  endcase
  c = 299792.458D

  gbu_file = 'GBU.'+wave_type+'.log'
  gbu = comp_read_gbu(gbu_file)
  ; only want the good 3pt measurements
  nthree = where(gbu.quality eq 'Good' and gbu.wavelengths eq 3, ng3)
  mg_log, '%d good files...', ng3, name='comp', /info
  if (ng3 eq 0) then goto, skip
  gbu = gbu[nthree]

  qu_files = intarr(n_elements(gbu))
  nt = n_elements(gbu)

  ; distinguish between Q/U files and V files
  for ii = 0L, nt - 1L do begin
    whatisthis = strmid(sxpar(headfits(gbu[ii].l1file, exten=4), 'EXTNAME'), 0, 1) 
    if (whatisthis eq 'Q') then qu_files[ii] = 1
  endfor

  for ii = 0L, nt - 1L do begin
    hdr    = headfits(gbu[ii].l1file)
    i1     = double(readfits(gbu[ii].l1file, exten_no=1, /silent, ehdr1))
    i2     = double(readfits(gbu[ii].l1file, exten_no=2, /silent, ehdr2))
    i3     = double(readfits(gbu[ii].l1file, exten_no=3, /silent, ehdr3))
    
    if (qu_files[ii] eq 1) then begin
      stks_q = double(readfits(gbu[ii].l1file, exten_no=4, /silent) $
                        + readfits(gbu[ii].l1file, exten_no=5, /silent) $
                        + readfits(gbu[ii].l1file, exten_no=6, /silent))
      stks_u = double(readfits(gbu[ii].l1file, exten_no=7, /silent) $
                        + readfits(gbu[ii].l1file, exten_no=8, /silent) $
                        + readfits(gbu[ii].l1file, exten_no=9, /silent))
    endif

    if (ii eq 0) then begin
      wavel    = [sxpar(ehdr1, 'WAVELENG'), $
                  sxpar(ehdr2, 'WAVELENG'), $
                  sxpar(ehdr3, 'WAVELENG')]
      index = fitshead2struct(hdr)
      d_lambda = double(mean(deriv(wavel)))
      vpix = (d_lambda / rest) * c    ; km/s/pix
      nx = (size(i1))[1]
      ny = (size(i1))[2]
      nz = (size(i1))[3] + 3
    endif

    if (ii gt 0) then index = merge_struct(index, fitshead2struct(hdr))
    temp_data = dblarr(nx, ny, 3)
    comp_make_mask, date, hdr, mask
    mask = double(mask)

    for xx = 0L, nx - 1L do begin
      for yy = 0L, ny - 1L do begin
        if (mask[xx,yy] eq 1) then begin
          ; compute analytical gaussfit
          profile = double([reform(i1[xx, yy]), $
                            reform(i2[xx, yy]), $
                            reform(i3[xx, yy])])
          sub_bad = where(profile le 0)
          if ((size(sub_bad))[0] eq 1) then profile[sub_bad] = 0.005D
          
          if (profile[1] gt int_thresh) then begin
            comp_analytic_gauss_fit, profile, d_lambda, doppler_shift, width, i_cent
          endif else begin
            i_cent        = 0D
            doppler_shift = 0D
            width         = 0D
          endelse
          if (abs(i_cent - profile[1]) gt diff_thresh) then begin
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
    thresh_masked = where(mask eq 1 and temp_int gt int_thresh, complement=thresh_unmasked)
    temp_velo = temp_data[*, *, 1]
    temp_velo[thresh_unmasked] = 0D

    pre_corr = dblarr(nx, ny, 2)
    pre_corr[*, *, 0] = temp_int
    pre_corr[*, *, 1] = temp_velo
    comp_doppler_correction, pre_corr, post_corr, wave_type, ewtrend, temptrend
    temp_corr_velo = reform(post_corr[*, *, 1])
    temp_velo = reform((temp_data[*, *, 1] - temptrend) / rest * c)
    temp_line_width = sqrt(2.) * (reform(temp_data[*, *, 2]) / d_lambda) * vpix   ; km/s

    temp_int[thresh_unmasked]        = 0D
    temp_velo[thresh_unmasked]       = 0D
    temp_corr_velo[thresh_unmasked]  = 0D
    temp_line_width[thresh_unmasked] = 0D
    if (qu_files[ii] eq 1) then begin
      stks_q[thresh_unmasked]          = 0D
      stks_u[thresh_unmasked]          = 0D
    endif

    ;=== write out fits files ===
    mg_log, 'write out fits %d', ii, name='comp', /info

    ;=== dynamics package ===
    primary_header = comp_convert_header(headfits(gbu[ii].l1file))
    outfilename = strmid(gbu[ii].l1file, 31, 26, /reverse_offset) + 'dynamics.3.fts'
    writefits, outfilename, blank, primary_header

    ; intensity
    extension_header = comp_convert_header(headfits(gbu[ii].l1file, exten=2), $
                                           /exten, $
                                           extname='Intensity', $
                                           datminmax=[min(temp_int), $
                                                      max(temp_int)])
    sxdelpar, extension_header, 'SIMPLE'
    writefits, outfilename, float(temp_int), extension_header, /append

    ; enhanced intensity
    extension_header = comp_convert_header(headfits(gbu[ii].l1file, exten=2), $
                                           /exten, $
                                           extname='Enhanced Intensity', $
                                           datminmax=[min(int_enh), $
                                                      max(int_enh)])
    sxdelpar, extension_header, 'SIMPLE'
    sxaddpar, extension_header, 'BITPIX', 8
    writefits, outfilename, int_enh, extension_header, /append

    ; corrected LOS velocity
    extension_header = comp_convert_header(headfits(gbu[ii].l1file, exten=2), $
                                           /exten,$
                                           extname='Corrected LOS velocity', $
                                           datminmax=[min(temp_corr_velo),$
                                                      max(temp_corr_velo)])
    sxdelpar, extension_header, 'SIMPLE'
    writefits, outfilename, float(temp_corr_velo), extension_header, /append

    ; line width
    extension_header = comp_convert_header(headfits(gbu[ii].l1file, exten=2), $
                                           /exten, $
                                           extname='Line Width', $
                                           datminmax=[min(temp_line_width), $
                                                      max(temp_line_width)])
    sxdelpar, extension_header, 'SIMPLE'
    writefits, outfilename, float(temp_line_width), extension_header, /append

    ;=== polarization package ===
    if (qu_files[ii] eq 1) then begin
      primary_header = comp_convert_header(headfits(gbu[ii].l1file))
      outfilename = strmid(gbu[ii].l1file, 31, 26, /reverse_offset) $
                      + 'polarization.3.fts'
      writefits, outfilename, blank, primary_header

      ; intensity
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, exten=2), $
                                             /exten, $
                                             extname='Intensity', $
                                             datminmax=[min(temp_int), $
                                                        max(temp_int)])
      sxdelpar, extension_header, 'SIMPLE'
      writefits, outfilename, float(temp_int), extension_header, /append

      ; enhanced intensity
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, exten=2), $
                                             /exten, $
                                             extname='Enhanced Intensity', $
                                             datminmax=[min(int_enh), $
                                                        max(int_enh)])
      sxdelpar, extension_header, 'SIMPLE'
      sxaddpar, extension_header, 'BITPIX', 8
      writefits, outfilename, int_enh, extension_header, /append

      ; Stokes Q
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, exten=2), $
                                             /exten,$
                                             extname='Integrated Stokes Q', $
                                             datminmax=[min(stks_q), $
                                                        max(stks_q)])
      sxdelpar, extension_header, 'SIMPLE'
      writefits, outfilename, float(stks_q), extension_header, /append

      ; Stokes U
      extension_header = comp_convert_header(headfits(gbu[ii].l1file,exten=2), $
                                             /exten, $
                                             extname='Integrated Stokes U', $
                                             datminmax=[min(stks_u), $
                                                        max(stks_u)])
      sxdelpar, extension_header, 'SIMPLE'
      writefits, outfilename, float(stks_u), extension_header, /append

      ; Linear Polarization
      lin_pol = sqrt((stks_q)^2. + (stks_u)^2.)
      extension_header = comp_convert_header(headfits(gbu[ii].l1file, exten=2), $
                                             /exten,$
                                             extname='Total Linear Polarization', $
                                             datminmax=[min(lin_pol), $
                                                        max(lin_pol)])
      sxdelpar, extension_header, 'SIMPLE'
      writefits, outfilename, float(lin_pol), extension_header, /append
    endif
  endfor

  skip:
  mg_log, 'done', name='comp', /info
end

