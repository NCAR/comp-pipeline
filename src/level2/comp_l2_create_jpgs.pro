; docformat = 'rst'

;+
; Creates the daily jpgs for the website from the L2 fits files, i.e., write
; out jpgs for hover and fullr for filesize issues, but pngs with a better
; quality for the dashboard.
;
; :Examples:
;   For example, call like::
;
;     comp_l2_create_jpgs, '20130520', '1074', nwl=5
;
; :Uses:
;   comp_constants_common, comp_config_common, comp_read_gbu, comp_uniq,
;   comp_make_mask, comp_aia_lct, comp_transparent_logo, colorbar2,
;   anytim2tai, sxpar, headfits, fitshead2struct, merge_struct, readfits,
;   mg_log
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :Keywords:
;    nwl : in, required, type=integer
;      number of lines, 3 or 5
;    n_avrg : in, optional, type=integer, default=50
;      number of files to average over
;
; :Author:
;   Christian Bethge
;
; :History:
;   removed gzip    Oct 1 2014  GdT
;-
pro comp_l2_create_jpgs, date_dir, wave_type, nwl=nwl, n_avrg=n_avrg
  compile_opt strictarr
  @comp_constants_common
  @comp_config_common

  mg_log, 'wave_type: %s %d points', wave_type, nwl, name='comp', /info

  l1_process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)
  l2_process_dir = filepath('', subdir=[date_dir, 'level2'], root=process_basedir)
  cd, l2_process_dir

  if (file_test('movies', /directory) eq 0) then file_mkdir, 'movies'

  nwlst = strcompress(string(nwl), /remove_all)
  
  rest = double(center1074)
  c = 299792.458D

  gbu_file = filepath('GBU.' + wave_type + '.log', root=l1_process_dir)
  if (~file_test(gbu_file)) then begin
    mg_log, '%s does not exist, skipping', gbu_file, name='comp', /warning
    goto, skip
  endif
  gbu = comp_read_gbu(gbu_file)
  for ii = 0L, n_elements(gbu) - 1L do begin
    gbu[ii].l1file = filepath(gbu[ii].l1file + '.gz', root=l1_process_dir)
  endfor

  ; only want the good measurements
  num_gf = where(gbu.quality eq 'Good' and gbu.wavelengths eq nwl, ng)
  mg_log, '%d good %d point files', ng, nwl, name='comp', /debug
  if (ng eq 0) then goto, skip
  gbu = gbu[num_gf]

  _n_avrg = n_elements(n_avrg) eq 0L ? 50L : n_avrg
  if (n_elements(gbu) gt _n_avrg) then begin
    gbu = gbu[0:_n_avrg-1]
  endif

  qu_files = intarr(n_elements(gbu))
  nt = n_elements(gbu)

  mg_log, 'using %d files...', nt, name='comp', /info

  ; distinguish between Q/U files and V files
  for ii = 0, nt - 1L do begin
    case nwl of
      3: whatisthis = strmid(sxpar(headfits(gbu[ii].l1file, exten=4), 'EXTNAME'), 0, 1)
      5: whatisthis = strmid(sxpar(headfits(gbu[ii].l1file, exten=6), 'EXTNAME'), 0, 1)
    endcase
    if (whatisthis eq 'Q') then qu_files[ii] = 1
  endfor

  for ii = 0, nt - 1L do begin
    hdr = headfits(gbu[ii].l1file)
    if (ii eq 0) then begin
      ehdr = headfits(gbu[ii].l1file, ext=1)
      index = fitshead2struct(hdr)
      nx = sxpar(ehdr, 'NAXIS1')
      ny = sxpar(ehdr, 'NAXIS2')
      nz = 7
      comp_data = fltarr(nx, ny, nz, nt)
    endif
    if (ii gt 0) then index = merge_struct(index, fitshead2struct(hdr))
    comp_make_mask, date_dir, hdr, mask
    mask = double(mask)

    l2_d_file = strmid(file_basename(gbu[ii].l1file), 0, 26) $
                  + 'dynamics.' + nwlst + '.fts.gz'
    l2_p_file = strmid(file_basename(gbu[ii].l1file), 0, 26) $
                  + 'polarization.' + nwlst + '.fts.gz'

    comp_data[*, *, 0, ii] = readfits(l2_d_file, ext=1, /silent)   ; Intensity
    comp_data[*, *, 1, ii] = readfits(l2_d_file, ext=2, /silent)   ; Enhanced Intensity
    comp_data[*, *, 2, ii] = readfits(l2_d_file, ext=3, /silent)   ; corrected LOS velocity
    comp_data[*, *, 3, ii] = readfits(l2_d_file, ext=4, /silent)   ; Line Width
    mg_log, '%s: %s file', l2_d_file, qu_files[ii] eq 1 ? 'QU' : 'V', $
            name='comp', /debug
    if (qu_files[ii] eq 1) then begin
      comp_data[*, *, 4, ii] = readfits(l2_p_file, ext=3, /silent)  ; integrated Stokes Q
      comp_data[*, *, 5, ii] = readfits(l2_p_file, ext=4, /silent)  ; integrated Stokes U
    endif else begin
      comp_data[*, *, 4, ii] = !values.f_nan
      comp_data[*, *, 5, ii] = !values.f_nan
    endelse
    comp_data[*, *, 6, ii] = mask   ; mask
  endfor

  obasefilename = filepath(date_dir + '.comp.' + wave_type, $
                           subdir='movies', $
                           root=l2_process_dir)

  ;=== prepare and write out daily images ===
  mg_log, 'creating daily JPGs now...', name='comp', /info

  mean_corr_data = reform(comp_data[*, *, *, 0] - comp_data[*, *, *, 0])
  for zz = 0, 5 do begin
    for yy = 0L, ny - 1L do begin
      for xx = 0L, nx - 1L do begin
        tmp_var = reform(comp_data[xx, yy, zz, *])
        good_val = where(finite(tmp_var) eq 1., n_finite_values)
        if (n_finite_values gt 0L) then begin
          mean_corr_data[xx, yy, zz] = median(tmp_var[good_val])
        endif else begin
          mean_corr_data[xx, yy, zz] = 0.
        endelse
      endfor
    endfor
  endfor

  comp_data  = 0
  mintensity = reform(mean_corr_data[*, *, 0])
  mint_enh   = reform(mean_corr_data[*, *, 1])
  velocity   = reform(mean_corr_data[*, *, 2])
  width      = reform(mean_corr_data[*, *, 3])

  stks_q     = reform(mean_corr_data[*, *, 4])
  stks_u     = reform(mean_corr_data[*, *, 5])
  p_angle    = sxpar(hdr, 'SOLAR_P0')
  azimuth    = (0.5 * atan(stks_u, stks_q) * 180. / !pi) - p_angle + 45.
  azimuth    = azimuth mod 180.
  bad_az     = where(azimuth lt 0.)
  if ((size(bad_az))[0] eq 1) then azimuth[bad_az] += 180.
  p          = sqrt(stks_q^2. + stks_u^2.)
  poi        = float(p) / float(mintensity)

  ; get the intensity from the original FITS files without the cosmetics (so
  ; that the full FOV is visible and no data is cut out for the website)
  intensity = fltarr(nx, ny)
  nmask = fltarr(nx, ny)
  for jj = 0L, nt - 1L do begin
    if (nwl eq 3) then intensity += readfits(gbu[jj].l1file, exten_no=2, /silent)
    if (nwl eq 5) then intensity += readfits(gbu[jj].l1file, exten_no=3, /silent)
    hdr = headfits(gbu[jj].l1file, ext=0)
    tmp_mask = fltarr(nx, ny)
    comp_make_mask, date_dir, hdr, ntmp_mask
    tmp_mask = ntmp_mask
    nmask += tmp_mask
  endfor
  intensity = float(intensity) / float(nt)
  nmask     = float(nmask) / float(nt)
  nmask[where(nmask ne 0)] = 1.

  for xx = 0L, nx - 1L do begin
    for yy = 0L, ny - 1L do begin
      if (nmask[xx,yy] eq 0) then intensity[xx,yy] = 0.
    endfor
  endfor

  thresh_masked = where(nmask eq 1 and mintensity gt int_thresh, $
                        complement=thresh_unmasked)

  int_enh   = mint_enh
  stks_q[thresh_unmasked]   = 0.
  stks_u[thresh_unmasked]   = 0.
  azimuth[thresh_unmasked]  = 0.
  poi[thresh_unmasked]      = 0.
  velocity[thresh_unmasked] = 0.
  width[thresh_unmasked]    = 0.

  ; get some info
  all_files = file_basename(file_search(filepath('*.comp.' + wave_type + '*.*.fts.gz', $
                                                 root=l1_process_dir), $
                                        count=no_of_files))
  no_of_files = n_elements(all_files)
  first_dt = strmid(all_files[0], 9, 6)
  first_file_time = string(strmid(first_dt, 0, 2), $
                           strmid(first_dt, 2, 2), $
                           strmid(first_dt, 4, 2), $
                           format='(%"%s:%s:%s UT")')
  last_dt = strmid(all_files[no_of_files - 1], 9, 6)
  last_file_time = string(strmid(last_dt, 0, 2), $
                          strmid(last_dt, 2, 2), $
                          strmid(last_dt, 4, 2), $
                          format='(%"%s:%s:%s UT")')

  ; now plot everything
  set_plot, 'Z'
  !p.font = 1
  erase, 255
  device, set_pixel_depth=24
  device, set_resolution=[4 * 485, 4 * 325]
  device, decomposed=0
  device, set_font='Helvetica', /tt_font
  tvlct, old_r, old_g, old_b, /get
  erase, 255

  bad_val = where(finite(velocity) ne 1)

  ; plot L_tot/I in b/w
  loadct, 0, /silent
  poi = alog10(poi)
  poi = bytscl(poi, min=-2.3, max=-0.3, /nan)
  if ((size(bad_val))[0] eq 1) then poi[bad_val] = 0.
  tv, poi, 4 * 5, 4 * 5

  ; plot doppler velocity
  restore, filepath('my_doppler_ct.sav', root=mg_src_root())
  tvlct, r, g, b
  vel = bytscl(velocity, min=-10, max=10, top=253)
  vel[thresh_unmasked] = 254
  if ((size(bad_val))[0] eq 1) then vel[bad_val] = 254
  tv, vel, 4 * 165, 4 * 5

  ; plot intensity and enhanced intensity
  comp_aia_lct, wave=193, /load
  int = sqrt(intensity)
  int = bytscl(int, min=1, max=5)
  tv, int, 4 * 5, 4 * 165
  tv, int_enh, 4 * 165, 4 * 165

  ; plot line width
  loadct, 4, /silent
  tvlct, r, g, b, /get
  b[255] = 0
  tvlct, r, g, b
  width = bytscl(width, min=25, max=55, top=254)
  if ((size(bad_val))[0] eq 1) then width[bad_val] = 0.
  tv, width, 4 * 325, 4 * 5

  ; print image info
  loadct, 0, /silent
  xyouts, 4 * 53, 4 * 238, 'Intensity', charsize=6, /device, color=255
  xyouts, 4 * 46 + 4 * 160, 4 * 245, 'Enhanced', charsize=6, /device, color=255
  xyouts, 4 * 53 + 4 * 160, 4 * 228, 'Intensity', charsize=6, /device, color=255
  xyouts, 4 * 66, 4 * 78, 'L!I tot !N/I', charsize=6, /device, color=255
  xyouts, 4 * 53 + 4 * 160., 4 * 97, 'Doppler', charsize=6, /device, color=255
  xyouts, 4 * 53.5 + 4 * 160, 4 * 78, 'Velocity', charsize=6, /device, color=255
  xyouts, 4 * 43.5 + 8 * 160, 4 * 78, 'Line Width', charsize=6, /device, color=255

  ; print info panel
  xyouts, 4 * 355, 4 * 140 + 4 * 160, 'MLSO/CoMP', charsize=6, /device, color=0
  xyouts, 4 * 349, 4 * 120 + 4 * 160, $
          'Date: ' + strmid(date_dir, 0, 4) + '/' + strmid(date_dir, 4, 2) $
            + '/' + strmid(date_dir, 6, 2),$
          charsize=5, /device, color=0
  xyouts, 4 * 326, 4 * 100 + 4 * 160, 'First image:', charsize=5, /device, color=0
  xyouts, 4 * 403, 4 * 100 + 4 * 160, first_file_time, charsize=5, /device, color=0
  xyouts, 4 * 326, 4 * 80 + 4 * 160, 'Last image:', charsize=5, /device, color=0
  xyouts, 4 * 403, 4 * 80 + 4 * 160, last_file_time, charsize=5, /device, color=0
  xyouts, 4 * 326, 4 * 60 + 4 * 160, 'Total # of images:  ' + strtrim(no_of_files, 2), $
          charsize=5, /device, color=0

  tvlct, rtemp, gtemp, btemp, /get

  ; read logos
  haologo_large = read_png(filepath('hao_logo.png', subdir='logos', root=mg_src_root()), $
                           rhao, ghao, bhao)
  haologo = read_png(filepath('hao_logo_small.png', subdir='logos', root=mg_src_root()), $
                     rhao, ghao, bhao)

  nsfimage_large  = read_png(filepath('nsf_ncar_logo.png', subdir='logos', root=mg_src_root()))
  nsfimage_large  = transpose(nsfimage_large, [1, 2, 0])
  nsfimsize_large = size(nsfimage_large[*, *, 0:2], /dimensions)
  nsfimage  = read_png(filepath('nsf_ncar_logo_small.png', subdir='logos', root=mg_src_root()))
  nsfimage  = transpose(nsfimage, [1, 2,0 ])
  nsfimsize = size(nsfimage[*, *, 0:2], /dimensions)

  nwimage  = read_png(filepath('nw_small.png', subdir='logos', root=mg_src_root()))
  nwimage  = transpose(nwimage, [1, 2, 0])
  nwimsize = size(nwimage[*, *, 0:2], /dimensions)

  ; display HAO logo
  tvlct, rhao, ghao, bhao
  tv, haologo_large, 4 * 336, 4 * 23 + 4 * 160
  tvlct, rtemp, gtemp, btemp

  ; display NSF/NCAR logo
  backgnd   = tvrd(4 * 404, 4 * 23 + 4 * 160, nsfimsize_large[0], nsfimsize_large[1], true=3)
  nsflogo   = comp_transparent_logo(nsfimage_large, backgnd)
  tv, nsflogo, true=3, 4 * 404, 4 * 23 + 4 * 160

  ; plot colorbar2s
  colorbar2, position=[0.091, 0.17, 0.091 + 0.158, 0.17 + 0.015], $
             charsize=1.25, title='log(L!Itot !N/I)', range=[-2.3, -0.3], $
             font=-1, divisions=4, format='(F5.1)'
  comp_aia_lct, wave=193, /load
  colorbar2, position=[0.092, 0.66, 0.092 + 0.158, 0.66 + 0.015], $
             charsize=1.25, title='sqrt(intensity)', range=[1, 5], font=-1, $
             divisions=4
  loadct, 4, /silent
  tvlct, r, g, b, /get
  b[255] = 255.
  tvlct, r, g, b
  colorbar2, position=[0.753, 0.17, 0.753 + 0.158, 0.17 + 0.015], $
             charsize=1.25, title='line width [km/s]', range=[25, 55], $
             font=-1, divisions=3, color=255, ncolors=254
  restore, filepath('my_doppler_ct.sav', root=mg_src_root())
  tvlct, r, g, b
  colorbar2, position=[0.4225, 0.17, 0.4225 + 0.158, 0.17 + 0.015], $
             charsize=1.25, title='LOS velocity [km/s]', range=[-10, 10], $
             font=-1, divisions=10, color=255, ncolors=254
  tvlct, old_r, old_g, old_b

  fhover = tvrd(/true)
  write_jpeg,  obasefilename + '.daily_fullr.' + nwlst + '.jpg', fhover, $
               true=1, quality=75
  hover = rebin(fhover, 3, 485, 325)
  write_jpeg, obasefilename + '.daily_hover.' + nwlst + '.jpg', hover, $
               true=1, quality=50

  ; plot the files for the dashboard
  device, set_pixel_depth=24
  device, set_resolution=[620, 620]
  device, decomposed=0
  device, set_font='Helvetica', /tt_font
  tvlct, old_r, old_g, old_b, /get
  colbarpos = [0.27, 0.31, 0.27 + 0.465, 0.31 + 0.03]

  ; plot L_tot/I in b/w
  loadct, 0, /silent
  tv, poi
  colorbar2, position=colbarpos, charsize=1.25, title='log(L!Itot !N/I)', $
             range=[-2.3, -0.3], font=-1, divisions=4, format='(F5.1)'
  xyouts, 4 * 62, 4 * 78, 'L!I tot !N/I', charsize=6, /device, color=255
  !p.font = -1
  xyouts, 4 * 1, 4 * 151.5, 'CoMP ' + wave_type, charsize=1, /device, color=255
  xyouts, 4 * 131, 4 * 151.5, $
          strmid(date_dir, 0, 4) + '-' + strmid(date_dir, 4, 2) $
            + '-' + strmid(date_dir,6,2), chars=1, /device, color=255
  !p.font = 1

  ; display HAO logo
  tvlct, rtemp, gtemp, btemp, /get
  tvlct, rhao, ghao, bhao
  tv, haologo
  tvlct, rtemp, gtemp, btemp

  ; display NSF/NCAR logo
  backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
  nsflogo   = comp_transparent_logo(nsfimage, backgnd)
  tv, nsflogo, true=3, 619 - 134, 0

  ; display N-W
  backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
  nwlogo   = comp_transparent_logo(nwimage, backgnd)
  tv, nwlogo, true=3, 4, 555
  ltot = tvrd(/true)
  erase

  ; plot Q/I in b/w
  loadct, 0, /silent
  qoi = float(stks_q) / float(mintensity)
  qoi[thresh_unmasked] = 0.
  qoi = bytscl(qoi, min=-0.3, max=0.3)
  qoi[thresh_unmasked] = 0.
  tv, qoi
  colorbar2, position=colbarpos, charsize=1.25, title='Q/I', $
             range=[-0.3, 0.3], font=-1, divisions=4, format='(F6.2)'
  xyouts, 4 * 66, 4 * 78, 'Q/I', chars=6, /device, color=255
  !p.font = -1
  xyouts, 4 * 1, 4 * 151.5, 'CoMP ' + wave_type, charsize=1, /device, color=255
  xyouts, 4 * 131, 4 * 151.5, $
          strmid(date_dir, 0, 4) + '-' + strmid(date_dir, 4, 2) $
            + '-' + strmid(date_dir, 6, 2), $
          charsize=1, /device, color=255
  !p.font = 1

  ; display HAO logo
  tvlct, rtemp, gtemp, btemp, /get
  tvlct, rhao, ghao, bhao
  tv, haologo
  tvlct, rtemp, gtemp, btemp

  ; display NSF/NCAR logo
  backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
  nsflogo   = comp_transparent_logo(nsfimage, backgnd)
  tv, nsflogo, true=3, 619 - 134, 0

  ; display N-W
  backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
  nwlogo   = comp_transparent_logo(nwimage, backgnd)
  tv, nwlogo, true=3, 4, 555
  qoveri = tvrd(/true)
  erase

  ; plot U/I in b/w
  loadct, 0, /silent
  uoi = float(stks_u) / float(mintensity)
  uoi[thresh_unmasked] = 0.
  uoi = bytscl(uoi, min=-0.3, max=0.3)
  uoi[thresh_unmasked] = 0.
  tv, uoi
  colorbar2, position=colbarpos, charsize=1.25, title='U/I', $
             range=[-0.3, 0.3], font=-1, divisions=4, format='(F6.2)'
  xyouts, 4 * 67, 4 * 78, 'U/I', charsize=6, /device, color=255
  !p.font = -1
  xyouts, 4 * 1, 4 * 151.5, 'CoMP ' + wave_type, charsize=1, /device, color=255
  xyouts, 4 * 131, 4 * 151.5, $
          strmid(date_dir, 0, 4) + '-' + strmid(date_dir, 4, 2) $
            + '-' + strmid(date_dir, 6, 2), $
          charsize=1, /device, color=255
  !p.font = 1

  ; display HAO logo
  tvlct, rtemp, gtemp, btemp, /get
  tvlct, rhao, ghao, bhao
  tv, haologo
  tvlct, rtemp, gtemp, btemp

  ; display NSF/NCAR logo
  backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
  nsflogo   = comp_transparent_logo(nsfimage, backgnd)
  tv, nsflogo, true=3, 619 - 134, 0

  ; display N-W
  backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
  nwlogo   = comp_transparent_logo(nwimage, backgnd)
  tv, nwlogo, true=3, 4, 555
  uoveri = tvrd(/true)
  erase

  ; plot doppler velocity
  restore, filepath('my_doppler_ct.sav', root=mg_src_root())
  tvlct, r, g, b
  tv, vel
  colorbar2, position=colbarpos, charsize=1.25, title='LOS velocity [km/s]', $
             range=[-10, 10], font=-1, divisions=10, color=255, ncolors=253
  loadct, 0, /silent
  xyouts, 4 * 48, 4 * 97, 'Doppler', charsize=6, /device, color=255
  xyouts, 4 * 48.5, 4 * 78, 'Velocity', charsize=6, /device, color=255
  !p.font = -1
  xyouts, 4 * 1, 4 * 151.5, 'CoMP ' + wave_type, charsize=1, /device, color=255
  xyouts, 4 * 131, 4 * 151.5, $
          strmid(date_dir, 0, 4) + '-' + strmid(date_dir, 4, 2) $
            + '-' + strmid(date_dir, 6, 2), $
          charsize=1, /device, color=255
  !p.font = 1

  ; display HAO logo
  tvlct, rtemp, gtemp, btemp, /get
  tvlct, rhao, ghao, bhao
  tv, haologo
  tvlct, rtemp, gtemp, btemp

  ; display NSF/NCAR logo
  backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
  nsflogo   = comp_transparent_logo(nsfimage, backgnd)
  tv, nsflogo, true=3, 619 - 134, 0

  ; display N-W
  backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
  nwlogo   = comp_transparent_logo(nwimage, backgnd)
  tv, nwlogo, true=3, 4, 555
  corr_velo = tvrd(/true)
  erase

  ; plot intensity
  comp_aia_lct, wave=193, /load
  tv, int
  colorbar2, position=colbarpos, charsize=1.25, title='sqrt(intensity)', $
             range=[1, 5], font=-1, divisions=4
  loadct, 0, /silent
  xyouts, 4 * 48, 4 * 78, 'Intensity', charsize=6, /device, color=255
  !p.font = -1
  xyouts, 4 * 1, 4 * 151.5, 'CoMP ' + wave_type, charsize=1, /device, color=255
  xyouts, 4 * 131, 4 * 151.5, $
          strmid(date_dir, 0, 4) + '-' + strmid(date_dir, 4, 2) $
            + '-' + strmid(date_dir, 6, 2), $
          charsize=1, /device, color=255
  !p.font = 1

  ; display HAO logo
  tvlct, rtemp, gtemp, btemp, /get
  tvlct, rhao, ghao, bhao
  tv, haologo
  tvlct, rtemp, gtemp, btemp

  ; display NSF/NCAR logo
  backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
  nsflogo   = comp_transparent_logo(nsfimage, backgnd)
  tv, nsflogo, true=3, 619 - 134, 0

  ; display N-W
  backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
  nwlogo   = comp_transparent_logo(nwimage, backgnd)
  tv, nwlogo, true=3, 4, 555
  intensity = tvrd(/true)
  erase

  ; plot enhanced intensity
  comp_aia_lct, wave=193, /load
  tv, int_enh
  loadct, 0, /silent
  xyouts, 4 * 40, 4 * 85, 'Enhanced', charsize=6, /device, color=255
  xyouts, 4 * 48, 4 * 68, 'Intensity', charsize=6, /device, color=255
  !p.font = -1
  xyouts, 4 * 1, 4 * 151.5, 'CoMP ' + wave_type, charsize=1, /device, color=255
  xyouts, 4 * 131, 4 * 151.5, $
          strmid(date_dir, 0, 4) + '-' + strmid(date_dir, 4, 2) $
            + '-' + strmid(date_dir,6 , 2), $
          charsize=1, /device, color=255
  !p.font = 1

  ; display HAO logo
  tvlct, rtemp, gtemp, btemp, /get
  tvlct, rhao, ghao, bhao
  tv, haologo
  tvlct, rtemp, gtemp, btemp

  ; display NSF/NCAR logo
  backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
  nsflogo   = comp_transparent_logo(nsfimage, backgnd)
  tv, nsflogo, true=3, 619-134, 0

  ; display N-W
  backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
  nwlogo   = comp_transparent_logo(nwimage, backgnd)
  tv, nwlogo, true=3, 4, 555
  enhanced_intensity = tvrd(/true)
  erase

  ; plot line width
  loadct, 4, /silent
  tvlct, r, g, b, /get
  b[255] = 255
  tvlct, r, g, b
  tv, width
  colorbar2, position=colbarpos, charsize=1.25, title='line width [km/s]',$
             range=[25, 55], font=-1, divisions=3, color=255, ncolors=254
  loadct, 0, /silent
  xyouts, 4 * 38, 4 * 78, 'Line Width', charsize=6, /device, color=255
  !p.font = -1
  xyouts, 4 * 1, 4 * 151.5, 'CoMP ' + wave_type, charsize=1, /device, color=255
  xyouts, 4 * 131, 4 * 151.5, $
          strmid(date_dir, 0, 4) + '-' + strmid(date_dir, 4, 2) $
            + '-' + strmid(date_dir, 6, 2), $
          charsize=1, /device, color=255
  !p.font = 1

  ; display HAO logo
  tvlct, rtemp, gtemp, btemp, /get
  tvlct, rhao, ghao, bhao
  tv, haologo
  tvlct, rtemp, gtemp, btemp

  ; display NSF/NCAR logo
  backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
  nsflogo   = comp_transparent_logo(nsfimage, backgnd)
  tv, nsflogo, true=3, 619 - 134, 0

  ; display N-W
  backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
  nwlogo   = comp_transparent_logo(nwimage, backgnd)
  tv, nwlogo, true=3, 4, 555
  line_width = tvrd(/true)
  erase

  ; plot azimuth
  loadct, 4, /silent
  tvlct, r, g, b, /get
  b[255] = 255
  tvlct, r, g, b
  azi = bytscl(azimuth, min=0, max=180, top=254)
  tv, azi
  colorbar2, position=colbarpos, charsize=1.25, title='Azimuth [degrees]',$
             range=[0, 180], font=-1, divisions=6, color=255, ncolors=254
  loadct, 0, /silent
  xyouts, 4 * 48, 4 * 78, 'Azimuth', charsize=6, /device, color=255
  !p.font = -1
  xyouts, 4 * 1, 4 * 151.5, 'CoMP ' + wave_type, charsize=1, /device, color=255
  xyouts, 4 * 131, 4 * 151.5, $
          strmid(date_dir, 0, 4) + '-' + strmid(date_dir, 4, 2) $
            + '-' + strmid(date_dir, 6, 2), $
          charsize=1, /device, color=255
  !p.font = 1

  ; display HAO logo
  tvlct, rtemp, gtemp, btemp, /get
  tvlct, rhao, ghao, bhao
  tv, haologo
  tvlct, rtemp, gtemp, btemp

  ; display NSF/NCAR logo
  backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
  nsflogo   = comp_transparent_logo(nsfimage, backgnd)
  tv, nsflogo, true=3, 619 - 134, 0

  ; display N-W
  backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
  nwlogo   = comp_transparent_logo(nwimage, backgnd)
  tv, nwlogo, true=3, 4, 555
  azimuth = tvrd(/true)
  erase

  write_png, obasefilename + '.daily_intensity.' + nwlst + '.png', intensity
  write_png, obasefilename + '.daily_enhanced_intensity.' + nwlst + '.png', $
             enhanced_intensity
  write_png, obasefilename + '.daily_corrected_velocity.' + nwlst + '.png', $
             corr_velo
  write_png, obasefilename + '.daily_line_width.' + nwlst + '.png', line_width
  write_png, obasefilename + '.daily_azimuth.' + nwlst + '.png', azimuth
  write_png, obasefilename + '.daily_ltot.' + nwlst + '.png', ltot
  write_png, obasefilename + '.daily_q.' + nwlst + '.png', qoveri
  write_png, obasefilename + '.daily_u.' + nwlst + '.png', uoveri

  ;  set_plot, 'X'
  ;  !p.font=-1
  ;  loadct, 0, /silent

  ;=== end of plotting daily images ===

  skip:
  mg_log, 'done', name='comp', /info
end

