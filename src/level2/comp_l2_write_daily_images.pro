; docformat = 'rst'

;+
; Creates the daily images for the website from the L2 fits files, i.e., write
; out JPEGs for hover and fullr for filesize issues, but PNGs with a better
; quality for the dashboard.
;
; :Examples:
;   For example, call like::
;
;     comp_l2_create_jpgs, '20130520', '1074'
;
; :Uses:
;   comp_constants_common, comp_config_common, comp_uniq,
;   comp_l2_mask, comp_aia_lct, comp_transparent_logo,
;   colorbar2, anytim2tai, sxpar, headfits, fitshead2struct, merge_struct,
;   readfits, mg_log
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :Keywords:
;   median : in, optional, type=boolean
;     set to use median files instead of mean files
;   waves : in, optional, type=boolean
;     set to explicitly use waves files, and produce waves output files
;   synoptic : in, optional, type=boolean
;     set to explicitly use synoptic files, and produce synoptic output files
;
; :Author:
;   MLSO Software Team
;
; :History:
;   Christian Bethge
;   removed gzip    Oct 1 2014  GdT
;   see git log for recent changes
;-
pro comp_l2_write_daily_images, date_dir, wave_type, $
                                median=median, waves=waves, synoptic=synoptic
  compile_opt strictarr
  @comp_constants_common
  @comp_config_common

  mg_log, 'wave_type: %s', wave_type, name='comp', /info

  l1_process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)
  l2_process_dir = filepath('', subdir=[date_dir, 'level2'], root=process_basedir)
  cd, l2_process_dir

  if (file_test('movies', /directory) eq 0) then file_mkdir, 'movies'

  averaging = keyword_set(median) ? 'median' : 'mean'

  ; read images from quick invert file
  waves_quick_invert_format = '(%"%s.comp.%s.quick_invert.%s.waves.fts.gz")'
  waves_quick_invert_filename = filepath(string(date_dir, wave_type, averaging, $
                                                format=waves_quick_invert_format), $
                                         root=l2_process_dir)

  synoptic_quick_invert_format = '(%"%s.comp.%s.quick_invert.%s.synoptic.fts.gz")'
  synoptic_quick_invert_filename = filepath(string(date_dir, wave_type, averaging, $
                                                   format=synoptic_quick_invert_format), $
                                            root=l2_process_dir)


  ; use quick invert images and eliminate reading L1 files

  waves_found = file_test(waves_quick_invert_filename)
  synoptic_found = file_test(synoptic_quick_invert_filename)

  if (keyword_set(waves) && keyword_set(synoptic)) then begin
    mg_log, 'both WAVES and SYNOPTIC keywords set', $
            name='comp', /error
    goto, done
  endif else if (keyword_set(waves)) then begin
    if (~waves_found) then begin
      mg_log, 'waves quick invert file %s not found', $
              file_basename(waves_quick_invert_filename), $
              name='comp', /debug
      goto, done
    endif
    dynamics_quick_invert_filename = waves_quick_invert_filename
    polarization_quick_invert_filename = waves_quick_invert_filename
  endif else if (keyword_set(synoptic)) then begin
    if (~synoptic_found) then begin
      mg_log, 'synoptic quick invert file %s not found', $
              file_basename(synoptic_quick_invert_filename), $
              name='comp', /debug
      goto, done
    endif
    dynamics_quick_invert_filename = synoptic_quick_invert_filename
    polarization_quick_invert_filename = synoptic_quick_invert_filename
  endif else begin
    if (~waves_found) then begin
      mg_log, 'waves quick invert file %s not found', $
              file_basename(waves_quick_invert_filename), $
              name='comp', /debug
    endif
    
    if (~synoptic_found) then begin
      mg_log, 'synoptic quick invert file %s not found', $
              file_basename(synoptic_quick_invert_filename), $
              name='comp', /debug
    endif
    
    if (~waves_found && ~synoptic_found) then begin
      mg_log, 'neither waves nor synoptic quick invert file found, skipping', $
              name='comp', /warn
      goto, done
    endif
    
    dynamics_quick_invert_filename = synoptic_found $
                                       ? synoptic_quick_invert_filename $
                                       : waves_quick_invert_filename
    polarization_quick_invert_filename = waves_found $
                                           ? waves_quick_invert_filename $
                                           : synoptic_quick_invert_filename
  endelse

  fits_open, dynamics_quick_invert_filename, dynamics_quick_invert_fcb

  fits_read, dynamics_quick_invert_fcb, intensity, intensity_header, exten_no=1, $
             /no_abort, message=msg
  if (msg ne '') then message, msg
  fits_read, dynamics_quick_invert_fcb, velocity, velocity_header, exten_no=6, $
             /no_abort, message=msg
  if (msg ne '') then message, msg
  fits_read, dynamics_quick_invert_fcb, width, width_header, exten_no=7, $
             /no_abort, message=msg
  if (msg ne '') then message, msg

  fits_close, dynamics_quick_invert_fcb

  fits_open, polarization_quick_invert_filename, polarization_quick_invert_fcb

  fits_read, polarization_quick_invert_fcb, stks_q, stks_q_header, exten_no=2, $
             /no_abort, message=msg
  if (msg ne '') then message, msg
  fits_read, polarization_quick_invert_fcb, stks_u, stks_u_header, exten_no=3, $
             /no_abort, message=msg
  if (msg ne '') then message, msg
  fits_read, polarization_quick_invert_fcb, lpol, lpol_header, exten_no=4, $
             /no_abort, message=msg
  if (msg ne '') then message, msg
  fits_read, polarization_quick_invert_fcb, azimuth, azimuth_header, exten_no=5, $
             /no_abort, message=msg
  if (msg ne '') then message, msg
  fits_read, polarization_quick_invert_fcb, $
             radial_azimuth, $
             radial_azimuth_header, $
             exten_no=8, $
             /no_abort, message=msg
  if (msg ne '') then message, msg

  fits_close, polarization_quick_invert_fcb

  ; create enhanced intensity
  enhanced_intensity = comp_intensity_enhancement(intensity, intensity_header, $
                                                  status=status, error_msg=error_msg)
  if (status le 0L) then begin
    mg_log, 'error creating enhanced intensity', name='comp', /warn
    mg_log, error_msg, name='comp', /warn
  endif

  ; prepare and write out daily images
  mg_log, 'creating daily JPGs now...', name='comp', /info

  mask = comp_l2_mask(intensity_header)
  good_ind = where(mask eq 1 $
                     and intensity gt int_min_thresh $
                     and intensity lt int_max_thresh, $
                   complement=mask_ind, ncomplement=n_mask_ind)

  p   = sqrt(stks_q^2. + stks_u^2.)
  poi = float(p) / float(intensity)

  stks_q[mask_ind] = 0.0
  stks_u[mask_ind] = 0.0
  azimuth[mask_ind] = 0.0
  poi[mask_ind] = 0.0
  ;velocity[mask_ind] = 0.0
  width[mask_ind] = 0.0

  ; get some info
  all_files = file_basename(file_search(filepath(date_dir + '.*.comp.' + wave_type $
                                                   + '*.*.fts.gz', $
                                                 root=l1_process_dir), $
                                        count=n_l1_files))

  first_dt = strmid(all_files[0], 9, 6)
  first_file_time = string(strmid(first_dt, 0, 2), $
                           strmid(first_dt, 2, 2), $
                           strmid(first_dt, 4, 2), $
                           format='(%"%s:%s:%s UT")')
  last_dt = strmid(all_files[n_l1_files - 1], 9, 6)
  last_file_time = string(strmid(last_dt, 0, 2), $
                          strmid(last_dt, 2, 2), $
                          strmid(last_dt, 4, 2), $
                          format='(%"%s:%s:%s UT")')

  ; now plot everything
  set_plot, 'Z'

  erase, 255
  device, set_pixel_depth=24
  device, set_resolution=[4 * 485, 4 * 325]
  device, decomposed=0
  device, set_font='Helvetica', /tt_font
  tvlct, old_r, old_g, old_b, /get
  erase, 255

  undef_velocity_ind = where(finite(velocity) ne 1, n_undef_velocity)

  ; plot L_tot/I in b/w
  loadct, 0, /silent
  poi = alog10(poi)
  poi = bytscl(poi, min=-2.3, max=-0.3, /nan)
  if (n_undef_velocity gt 0) then poi[undef_velocity_ind] = 0.
  tv, poi, 4 * 5, 4 * 5

  ; plot doppler velocity
  restore, filepath('my_doppler_ct.sav', root=mg_src_root())
  tvlct, r, g, b
  vel = bytscl(velocity, min=-10, max=10, top=253)
  vel[mask_ind] = 254
  if (n_undef_velocity gt 0L) then vel[undef_velocity_ind] = 254
  tv, vel, 4 * 165, 4 * 5

  ; plot intensity and enhanced intensity
  comp_aia_lct, wave=193, /load
  int = sqrt(intensity)
  display_min_i = 0.3
  case wave_type of
    '1074': display_max_i = 3.0
    '1079': display_max_i = 2.0
    else: display_max_i = 3.0
  endcase
  int = bytscl(int, min=display_min_i, max=display_max_i)
  tv, int, 4 * 5, 4 * 165
  tv, enhanced_intensity, 4 * 165, 4 * 165

  ; plot line width
  loadct, 4, /silent
  tvlct, rgb, /get
  rgb[254, *] = 0
  rgb[255, *] = 255
  tvlct, rgb
  width = bytscl(width, min=25, max=55, top=254)
  if (n_undef_velocity gt 0) then width[undef_velocity_ind] = 254
  tv, width, 4 * 325, 4 * 5

  ; print image info
  loadct, 0, /silent
  xyouts, 4 * 53, 4 * 238, 'Intensity', charsize=6, /device, color=255, font=1
  xyouts, 4 * 46 + 4 * 160, 4 * 245, 'Enhanced', charsize=6, /device, $
          color=255, font=1
  xyouts, 4 * 53 + 4 * 160, 4 * 228, 'Intensity', charsize=6, /device, $
          color=255, font=1
  xyouts, 4 * 66, 4 * 78, 'L!I tot !N/I', charsize=6, /device, color=255, font=1
  xyouts, 4 * 53 + 4 * 160., 4 * 97, 'Doppler', charsize=6, /device, $
          color=255, font=1
  xyouts, 4 * 53.5 + 4 * 160, 4 * 78, 'Velocity', charsize=6, /device, $
          color=255, font=1
  xyouts, 4 * 43.5 + 8 * 160, 4 * 78, 'Line Width', charsize=6, /device, $
          color=255, font=1

  ; print info panel
  xyouts, 4 * 355, 4 * 140 + 4 * 160, 'MLSO/CoMP', charsize=6, /device, color=0, font=1
  xyouts, 4 * 349, 4 * 120 + 4 * 160, $
          'Date: ' + strmid(date_dir, 0, 4) + '/' + strmid(date_dir, 4, 2) $
            + '/' + strmid(date_dir, 6, 2),$
          charsize=5, /device, color=0, font=1
  xyouts, 4 * 326, 4 * 100 + 4 * 160, 'First image:', charsize=5, /device, color=0, $
          font=1
  xyouts, 4 * 403, 4 * 100 + 4 * 160, first_file_time, charsize=5, /device, color=0, $
          font=1
  xyouts, 4 * 326, 4 * 80 + 4 * 160, 'Last image:', charsize=5, /device, color=0, $
          font=1
  xyouts, 4 * 403, 4 * 80 + 4 * 160, last_file_time, charsize=5, /device, $
          color=0, font=1
  xyouts, 4 * 326, 4 * 60 + 4 * 160, 'Total # of images:  ' + strtrim(n_l1_files, 2), $
          charsize=5, /device, color=0, font=1

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
             charsize=1.25, title='sqrt(intensity)', $
             range=[display_min_i, display_max_i], format='(F0.1)', font=-1, $
             divisions=4
  loadct, 4, /silent
  tvlct, rgb, /get
  rgb[254, *] = 0
  rgb[255, *] = 255
  tvlct, rgb
  colorbar2, position=[0.753, 0.17, 0.753 + 0.158, 0.17 + 0.015], $
             charsize=1.25, title='line width [km/s]', range=[25, 55], $
             font=-1, divisions=3, color=255, ncolors=254
  restore, filepath('my_doppler_ct.sav', root=mg_src_root())
  tvlct, r, g, b
  colorbar2, position=[0.4225, 0.17, 0.4225 + 0.158, 0.17 + 0.015], $
             charsize=1.25, title='LOS velocity [km/s]', range=[-10, 10], $
             font=-1, divisions=10, color=255, ncolors=254
  tvlct, old_r, old_g, old_b

  obasefilename = filepath(date_dir + '.comp.' + wave_type, $
                           subdir='movies', $
                           root=l2_process_dir)

  fhover = tvrd(/true)
  write_jpeg,  obasefilename + '.daily_fullr.jpg', fhover, $
               true=1, quality=75

  hover = rebin(fhover, 3, 485, 325)
  write_jpeg, obasefilename + '.daily_hover.jpg', hover, $
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
  xyouts, 4 * 62, 4 * 78, 'L!I tot !N/I', charsize=6, /device, color=255, font=1

  xyouts, 4 * 1, 4 * 151.5, 'MLSO CoMP ' + wave_type, charsize=1, /device, color=255
  xyouts, 4 * 131, 4 * 151.5, $
          strmid(date_dir, 0, 4) + '-' + strmid(date_dir, 4, 2) $
            + '-' + strmid(date_dir,6,2), chars=1, /device, color=255

  ; display HAO logo
  ;tvlct, rtemp, gtemp, btemp, /get
  ;tvlct, rhao, ghao, bhao
  ;tv, haologo
  ;tvlct, rtemp, gtemp, btemp
  xyouts, 4 * 1, 4 * 1, 'NSF NCAR/HAO', $
          charsize=1.0, /device, color=255

  ; display NSF/NCAR logo
  ;backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
  ;nsflogo   = comp_transparent_logo(nsfimage, backgnd)
  ;tv, nsflogo, true=3, 619 - 134, 0

  ; display N-W
  backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
  nwlogo   = comp_transparent_logo(nwimage, backgnd)
  tv, nwlogo, true=3, 4, 555
  ltot = tvrd(/true)
  erase

  ; plot Q/I in b/w
  loadct, 0, /silent
  qoi = float(stks_q) / float(intensity)
  qoi[mask_ind] = 0.
  display_min_q = -0.1
  display_max_q = 0.1
  qoi = bytscl(qoi, min=display_min_q, max=display_max_q)
  qoi[mask_ind] = 0B
  tv, qoi
  colorbar2, position=colbarpos, charsize=1.25, title='Q/I', $
             range=[display_min_q, display_max_q], font=-1, divisions=4, format='(F6.2)'
  xyouts, 4 * 66, 4 * 78, 'Q/I', chars=6, /device, color=255, font=1

  xyouts, 4 * 1, 4 * 151.5, 'MLSO CoMP ' + wave_type, charsize=1, /device, color=255
  xyouts, 4 * 131, 4 * 151.5, $
          strmid(date_dir, 0, 4) + '-' + strmid(date_dir, 4, 2) $
            + '-' + strmid(date_dir, 6, 2), $
          charsize=1, /device, color=255

  ; display HAO logo
  ;tvlct, rtemp, gtemp, btemp, /get
  ;tvlct, rhao, ghao, bhao
  ;tv, haologo
  ;tvlct, rtemp, gtemp, btemp
  xyouts, 4 * 1, 4 * 1, 'NSF NCAR/HAO', $
          charsize=1.0, /device, color=255

  ; display NSF/NCAR logo
  ;backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
  ;nsflogo   = comp_transparent_logo(nsfimage, backgnd)
  ;tv, nsflogo, true=3, 619 - 134, 0

  ; display N-W
  backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
  nwlogo   = comp_transparent_logo(nwimage, backgnd)
  tv, nwlogo, true=3, 4, 555
  qoveri = tvrd(/true)
  erase

  ; plot U/I in b/w
  loadct, 0, /silent
  uoi = float(stks_u) / float(intensity)
  uoi[mask_ind] = 0.
  display_min_u = -0.1
  display_max_u = 0.1
  uoi = bytscl(uoi, min=display_min_u, max=display_max_u)
  uoi[mask_ind] = 0.
  tv, uoi
  colorbar2, position=colbarpos, charsize=1.25, title='U/I', $
             range=[display_min_u, display_max_u], font=-1, divisions=4, format='(F6.2)'
  xyouts, 4 * 67, 4 * 78, 'U/I', charsize=6, /device, color=255, font=1

  xyouts, 4 * 1, 4 * 151.5, 'MLSO CoMP ' + wave_type, charsize=1, /device, color=255
  xyouts, 4 * 131, 4 * 151.5, $
          strmid(date_dir, 0, 4) + '-' + strmid(date_dir, 4, 2) $
            + '-' + strmid(date_dir, 6, 2), $
          charsize=1, /device, color=255

  ; display HAO logo
  ;tvlct, rtemp, gtemp, btemp, /get
  ;tvlct, rhao, ghao, bhao
  ;tv, haologo
  ;tvlct, rtemp, gtemp, btemp
  xyouts, 4 * 1, 4 * 1, 'NSF NCAR/HAO', $
          charsize=1.0, /device, color=255

  ; display NSF/NCAR logo
  ;backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
  ;nsflogo   = comp_transparent_logo(nsfimage, backgnd)
  ;tv, nsflogo, true=3, 619 - 134, 0

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
  xyouts, 4 * 48, 4 * 97, 'Doppler', charsize=6, /device, color=255, font=1
  xyouts, 4 * 48.5, 4 * 78, 'Velocity', charsize=6, /device, color=255, font=1

  xyouts, 4 * 1, 4 * 151.5, 'MLSO CoMP ' + wave_type, charsize=1, /device, color=255
  xyouts, 4 * 131, 4 * 151.5, $
          strmid(date_dir, 0, 4) + '-' + strmid(date_dir, 4, 2) $
            + '-' + strmid(date_dir, 6, 2), $
          charsize=1, /device, color=255

  ; display HAO logo
  ;tvlct, rtemp, gtemp, btemp, /get
  ;tvlct, rhao, ghao, bhao
  ;tv, haologo
  ;tvlct, rtemp, gtemp, btemp
  xyouts, 4 * 1, 4 * 1, 'NSF NCAR/HAO', $
          charsize=1.0, /device, color=255

  ; display NSF/NCAR logo
  ;backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
  ;nsflogo   = comp_transparent_logo(nsfimage, backgnd)
  ;tv, nsflogo, true=3, 619 - 134, 0

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
             range=[display_min_i, display_max_i], format='(F0.1)', font=-1, divisions=4
  loadct, 0, /silent
  xyouts, 4 * 48, 4 * 78, 'Intensity', charsize=6, /device, color=255, font=1

  xyouts, 4 * 1, 4 * 151.5, 'MLSO CoMP ' + wave_type, charsize=1, /device, color=255
  xyouts, 4 * 131, 4 * 151.5, $
          strmid(date_dir, 0, 4) + '-' + strmid(date_dir, 4, 2) $
            + '-' + strmid(date_dir, 6, 2), $
          charsize=1, /device, color=255

  ; display HAO logo
  ;tvlct, rtemp, gtemp, btemp, /get
  ;tvlct, rhao, ghao, bhao
  ;tv, haologo
  ;tvlct, rtemp, gtemp, btemp
  xyouts, 4 * 1, 4 * 1, 'NSF NCAR/HAO', $
          charsize=1.0, /device, color=255

  ; display NSF/NCAR logo
  ;backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
  ;nsflogo   = comp_transparent_logo(nsfimage, backgnd)
  ;tv, nsflogo, true=3, 619 - 134, 0

  ; display N-W
  backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
  nwlogo   = comp_transparent_logo(nwimage, backgnd)
  tv, nwlogo, true=3, 4, 555
  intensity = tvrd(/true)
  erase

  ; plot enhanced intensity
  comp_aia_lct, wave=193, /load
  tv, enhanced_intensity
  loadct, 0, /silent
  xyouts, 4 * 40, 4 * 85, 'Enhanced', charsize=6, /device, color=255, font=1
  xyouts, 4 * 48, 4 * 68, 'Intensity', charsize=6, /device, color=255, font=1

  xyouts, 4 * 1, 4 * 151.5, 'MLSO CoMP ' + wave_type, charsize=1, /device, color=255
  xyouts, 4 * 131, 4 * 151.5, $
          strmid(date_dir, 0, 4) + '-' + strmid(date_dir, 4, 2) $
            + '-' + strmid(date_dir,6 , 2), $
          charsize=1, /device, color=255

  ; display HAO logo
  ;tvlct, rtemp, gtemp, btemp, /get
  ;tvlct, rhao, ghao, bhao
  ;tv, haologo
  ;tvlct, rtemp, gtemp, btemp
  xyouts, 4 * 1, 4 * 1, 'NSF NCAR/HAO', $
          charsize=1.0, /device, color=255

  ; display NSF/NCAR logo
  ;backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
  ;nsflogo   = comp_transparent_logo(nsfimage, backgnd)
  ;tv, nsflogo, true=3, 619-134, 0

  ; display N-W
  backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
  nwlogo   = comp_transparent_logo(nwimage, backgnd)
  tv, nwlogo, true=3, 4, 555
  enhanced_intensity = tvrd(/true)
  erase

  ; plot line width
  loadct, 4, /silent
  tvlct, rgb, /get
  rgb[254, *] = 0
  rgb[255, *] = 255
  tvlct, rgb
  tv, width

  colorbar2, position=colbarpos, charsize=1.25, title='line width [km/s]',$
             range=[25, 55], font=-1, divisions=3, color=255, ncolors=254
  loadct, 0, /silent
  xyouts, 4 * 38, 4 * 78, 'Line Width', charsize=6, /device, color=255, font=1

  xyouts, 4 * 1, 4 * 151.5, 'MLSO CoMP ' + wave_type, charsize=1, /device, color=255
  xyouts, 4 * 131, 4 * 151.5, $
          strmid(date_dir, 0, 4) + '-' + strmid(date_dir, 4, 2) $
            + '-' + strmid(date_dir, 6, 2), $
          charsize=1, /device, color=255

  ; display HAO logo
  ;tvlct, rtemp, gtemp, btemp, /get
  ;tvlct, rhao, ghao, bhao
  ;tv, haologo
  ;tvlct, rtemp, gtemp, btemp
  xyouts, 4 * 1, 4 * 1, 'NSF NCAR/HAO', $
          charsize=1.0, /device, color=255

  ; display NSF/NCAR logo
  ;backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
  ;nsflogo   = comp_transparent_logo(nsfimage, backgnd)
  ;tv, nsflogo, true=3, 619 - 134, 0

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
  xyouts, 4 * 48, 4 * 78, 'Azimuth', charsize=6, /device, color=255, font=1

  xyouts, 4 * 1, 4 * 151.5, 'MLSO CoMP ' + wave_type, charsize=1, /device, color=255
  xyouts, 4 * 131, 4 * 151.5, $
          strmid(date_dir, 0, 4) + '-' + strmid(date_dir, 4, 2) $
            + '-' + strmid(date_dir, 6, 2), $
          charsize=1, /device, color=255

  ; display HAO logo
  ;tvlct, rtemp, gtemp, btemp, /get
  ;tvlct, rhao, ghao, bhao
  ;tv, haologo
  ;tvlct, rtemp, gtemp, btemp
  xyouts, 4 * 1, 4 * 1, 'NSF NCAR/HAO', $
          charsize=1.0, /device, color=255

  ; display NSF/NCAR logo
  ;backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
  ;nsflogo   = comp_transparent_logo(nsfimage, backgnd)
  ;tv, nsflogo, true=3, 619 - 134, 0

  ; display N-W
  backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
  nwlogo   = comp_transparent_logo(nwimage, backgnd)
  tv, nwlogo, true=3, 4, 555
  azimuth = tvrd(/true)
  erase

  ; plot radial azimuth
  ncolors = 256 - 1 - 1   ; one for annotation color, one for bad data
  loadct, 6, /silent, ncolors=ncolors
  tvlct, r, g, b, /get
  r[0:ncolors - 1] = shift(r[0:ncolors - 1], ncolors / 2)
  g[0:ncolors - 1] = shift(g[0:ncolors - 1], ncolors / 2)
  b[0:ncolors - 1] = shift(b[0:ncolors - 1], ncolors / 2)
  tvlct, r, g, b
  ;tvlct, 128B, 128B, 128B, 254L   ; bad values are grey
  tvlct, 0B, 0B, 0B, 254L   ; bad values are black
  tvlct, 255B, 255B, 255B, 255L   ; annotation color is white
  bad_ind = where(radial_azimuth lt -90, n_bad_ind)
  rad_azi = bytscl(radial_azimuth, min=-90.0, max=90.0, top=ncolors - 1)
  if (n_bad_ind gt 0L) then rad_azi[bad_ind] = 254B
  if (n_mask_ind gt 0L) then rad_azi[mask_ind] = 254B
  tv, rad_azi
  colorbar2, position=colbarpos, charsize=1.25, title='Radial Azimuth [degrees]',$
             range=[-90, 90], font=-1, divisions=6, color=255, ncolors=ncolors
  loadct, 0, /silent
  xyouts, 620 / 2, 4 * 78, 'Radial Azimuth', charsize=6, /device, color=255, $
          alignment=0.5, font=1

  xyouts, 4 * 1, 4 * 151.5, 'MLSO CoMP ' + wave_type, charsize=1, /device, color=255
  xyouts, 4 * 131, 4 * 151.5, $
          strmid(date_dir, 0, 4) + '-' + strmid(date_dir, 4, 2) $
            + '-' + strmid(date_dir, 6, 2), $
          charsize=1, /device, color=255

  ; display HAO logo
  ;tvlct, rtemp, gtemp, btemp, /get
  ;tvlct, rhao, ghao, bhao
  ;tv, haologo
  ;tvlct, rtemp, gtemp, btemp
  xyouts, 4 * 1, 4 * 1, 'NSF NCAR/HAO', $
          charsize=1.0, /device, color=255

  ; display NSF/NCAR logo
  ;backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
  ;nsflogo   = comp_transparent_logo(nsfimage, backgnd)
  ;tv, nsflogo, true=3, 619 - 134, 0

  ; display N-W
  backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
  nwlogo   = comp_transparent_logo(nwimage, backgnd)
  tv, nwlogo, true=3, 4, 555
  radial_azimuth = tvrd(/true)
  erase

  modifier = ''
  if (keyword_set(median)) then modifier += '.median'
  if (keyword_set(waves)) then modifier += '.waves'
  if (keyword_set(synoptic)) then modifier += '.synoptic'
  obasefilename += modifier

  write_png, obasefilename + '.daily_intensity.png', intensity
  write_png, obasefilename + '.daily_enhanced_intensity.png', $
             enhanced_intensity
  write_png, obasefilename + '.daily_corrected_velocity.png', $
             corr_velo
  write_png, obasefilename + '.daily_line_width.png', line_width
  write_png, obasefilename + '.daily_azimuth.png', azimuth
  write_png, obasefilename + '.daily_radial_azimuth.png', radial_azimuth
  write_png, obasefilename + '.daily_ltot.png', ltot
  write_png, obasefilename + '.daily_q.png', qoveri
  write_png, obasefilename + '.daily_u.png', uoveri

  ; end of plotting daily images

  done:
  mg_log, 'done', name='comp', /info
end


; main-level example program

date = '20170104'
config_filename = filepath('comp.latest.cfg', $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())

comp_initialize, date
comp_configuration, config_filename=config_filename

comp_l2_write_daily_images, date, '1074'
comp_l2_write_daily_images, date, '1074', /median, /waves
comp_l2_write_daily_images, date, '1074', /median, /synoptic

end
