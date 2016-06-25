; docformat = 'rst'

;+
; Creates the daily mp4 movies for the website from the L2 fits files.
;
; :Examples:
;   For example, call it like::
;
;     comp_l2_create_movies, '20120708', '1074', nwl=3
;
; :Uses:
;   comp_constants_common, comp_config_common, comp_read_gbu, comp_make_mask,
;   comp_transparent_logo, comp_aia_lct, colorbar2,
;   sxpar, headfits, fitshead2struct, merge_struct, readfits, 
;   mg_log
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
pro comp_l2_create_movies, date_dir, wave_type, nwl=nwl
  compile_opt strictarr
  @comp_constants_common
  @comp_config_common

  mg_log, 'wave_type: %s %2d', wave_type, nwl, name='comp', /info

  nwlst = strcompress(string(nwl), /remove_all)

  l1_process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)
  l2_process_dir = filepath('', subdir=[date_dir, 'level2'], root=process_basedir)
  cd, l2_process_dir

  temp_path = 'movies'
  if (file_test(temp_path, /directory) eq 0) then file_mkdir, temp_path

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
  mg_log, '%d good files...', ng, name='comp', /info

  if (ng eq 0) then goto, skip
  gbu = gbu[num_gf]

  qu_files = intarr(n_elements(gbu))
  nt = n_elements(gbu)
  p_counter = 0

  ; distinguish between Q/U files and V files
  for ii = 0, nt - 1L do begin
    case nwl of
      3: whatisthis = strmid(sxpar(headfits(gbu[ii].l1file, exten=4), $
                                   'EXTNAME'), $
                             0, 1)
      5: whatisthis = strmid(sxpar(headfits(gbu[ii].l1file, exten=6), $
                                   'EXTNAME'), $
                             0, 1)
    endcase
    if (whatisthis eq 'Q') then qu_files[ii] = 1
  endfor

  ; read logos
  haologo = read_png(filepath('hao_logo_small.png', subdir='logos', root=mg_src_root()), $
                     rhao, ghao, bhao)

  nsfimage  = read_png(filepath('nsf_ncar_logo_small.png', subdir='logos', root=mg_src_root()))
  nsfimage  = transpose(nsfimage, [1, 2, 0])
  nsfimsize = size(nsfimage[*, *, 0:2], /dimensions)

  nwimage  = read_png(filepath('nw_small.png', subdir='logos', root=mg_src_root()))
  nwimage  = transpose(nwimage, [1, 2, 0])
  nwimsize = size(nwimage[*, *, 0:2], /dimensions)

  for ii = 0L, nt - 1L do begin
    mg_log, '%d/%d: %s', $
            ii + 1L, nt, file_basename(gbu[ii].l1file), $
            name='comp', /info
    hdr = headfits(gbu[ii].l1file)
    if (ii eq 0) then begin
      index = fitshead2struct(hdr)
      ehdr = headfits(gbu[ii].l1file, ext=1)
      nx = sxpar(ehdr, 'NAXIS1')
      ny = sxpar(ehdr, 'NAXIS2')
    endif
    if (ii gt 0) then index = merge_struct(index, fitshead2struct(hdr))
    comp_make_mask, date_dir, hdr, mask
    mask = double(mask)

    l2_d_file = strmid(file_basename(gbu[ii].l1file), 0, 26) $
                  + 'dynamics.' + nwlst + '.fts.gz'
    l2_p_file = strmid(file_basename(gbu[ii].l1file), 0, 26) $
                  + 'polarization.' + nwlst + '.fts.gz'

    intensity = readfits(l2_d_file, ext=1, /silent)   ; Intensity
    int_enh   = readfits(l2_d_file, ext=2, /silent)   ; Enhanced Intensity
    velocity  = readfits(l2_d_file, ext=3, /silent)   ; corrected LOS velocity
    width     = readfits(l2_d_file, ext=4, /silent)   ; Line Width
    if (qu_files[ii] eq 1) then begin
      stks_q = readfits(l2_p_file, ext=3, /silent)   ; integrated Stokes Q
      stks_u = readfits(l2_p_file, ext=4, /silent)   ; integrated Stokes U
      ltot   = readfits(l2_p_file, ext=5, /silent)   ; integrated L_tot
    endif

    set_plot, 'Z'
    !p.font = 1
    device, set_pixel_depth=24
    device, set_resolution=[620, 620]
    device, decomposed=0
    device, set_font='Helvetica', /tt_font
    tvlct, old_r, old_g, old_b, /get
    colbarpos = [0.27, 0.31, 0.27 + 0.465, 0.31 + 0.03]

    bad_val = where(finite(velocity) ne 1)
    comp_mask = mask
    masked = where(comp_mask eq 1 and intensity gt int_thresh, comp=unmasked)

    ; plot Q/I in b/w
    loadct, 0, /silent
    if (qu_files[ii] eq 1) then begin
      qoi = stks_q / intensity
      qoi[unmasked] = 0.
      qoi = bytscl(qoi, min=-0.15, max=0.15)
      qoi[unmasked] = 0.
      tv, qoi
      colorbar2, position=colbarpos, chars=1.25, title='Q/I', $
                 range=[-0.15, 0.15], font=-1, divisions=4, format='(F6.3)'
      xyouts, 4 * 66, 4 * 78, 'Q/I', chars=6, /device, color=255
      !p.font = -1
      xyouts, 4 * 1, 4 * 151.5, 'CoMP ' + wave_type, chars=1, /device, color=255
      xyouts, 4 * 109, 4 * 151.5, $
              index[ii].date_d$obs + ' ' + index[ii].time_d$obs + ' UT', $
              chars=1, /device, color=255
      !p.font = 1

      ; display HAO logo
      tvlct, rtemp, gtemp, btemp, /get
      tvlct, rhao, ghao, bhao
      tv, haologo
      tvlct, rtemp, gtemp, btemp

      ; display NSF/NCAR logo
      backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
      nsflogo   = comp_transparent_logo(nsfimage, backgnd, 619 - 134, 0)
      tv, nsflogo, true=3, 619 - 134, 0

      ; display N-W
      backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
      nwlogo   = comp_transparent_logo(nwimage, backgnd, 4, 555)
      tv, nwlogo, true=3, 4, 555
      qoveri = tvrd(/true)
      erase
    endif

    ; plot U/I in b/w
    loadct, 0, /silent
    if (qu_files[ii] eq 1) then begin
      uoi = stks_u/intensity
      uoi[unmasked] = 0.
      uoi = bytscl(uoi, min=-0.15, max=0.15)
      uoi[unmasked] = 0.
      tv, uoi
      colorbar2, position=colbarpos,chars=1.25, title='U/I', $
                 range=[-0.15, 0.15], font=-1, divisions=4, format='(F6.3)'
      xyouts, 4 * 67, 4 * 78, 'U/I', chars=6, /device, color=255
      !p.font = -1
      xyouts, 4 * 1, 4 * 151.5, 'CoMP ' + wave_type, chars=1, /device, color=255
      xyouts, 4 * 109, 4 * 151.5, $
              index[ii].date_d$obs + ' ' + index[ii].time_d$obs + ' UT', $
              chars=1, /device, color=255
      !p.font = 1

      ; display HAO logo
      tvlct, rtemp, gtemp, btemp, /get
      tvlct, rhao, ghao, bhao
      tv, haologo
      tvlct, rtemp, gtemp, btemp

      ; display NSF/NCAR logo
      backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
      nsflogo   = comp_transparent_logo(nsfimage, backgnd, 619 - 134, 0)
      tv, nsflogo, true=3, 619 - 134, 0

      ; display N-W
      backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
      nwlogo   = comp_transparent_logo(nwimage, backgnd, 4, 555)
      tv, nwlogo, true=3, 4, 555
      uoveri = tvrd(/true)
      erase
    endif

    ; plot L_tot/I in b/w
    loadct, 0, /silent
    if (qu_files[ii] eq 1) then begin
      ++p_counter
      poi = ltot / intensity
      poi[unmasked] = 0
      poi = alog10(poi)
      poi = bytscl(poi, min=-2.3, max=-0.3, /nan)
      if ((size(bad_val))[0] eq 1) then poi[bad_val] = 0
      tv, poi
      colorbar2, position=colbarpos,chars=1.25, title='log(L!Itot !N/I)', $
                 range=[-2.3, -0.3], font=-1, divisions=4, format='(F5.1)'
      xyouts, 4 * 62, 4 * 78, 'L!I tot !N/I', chars=6, /device, color=255
      !p.font = -1
      xyouts, 4 * 1, 4 * 151.5, 'CoMP ' + wave_type, chars=1, /device, color=255
      xyouts, 4 * 109, 4 * 151.5, $
              index[ii].date_d$obs + ' ' + index[ii].time_d$obs + ' UT', $
              chars=1, /device, color=255
      !p.font = 1

      ; display HAO logo
      tvlct, rtemp, gtemp, btemp, /get
      tvlct, rhao, ghao, bhao
      tv, haologo
      tvlct, rtemp, gtemp, btemp

      ; display NSF/NCAR logo
      backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
      nsflogo   = comp_transparent_logo(nsfimage,backgnd, 619 - 134, 0)
      tv, nsflogo, true=3, 619 - 134, 0

      ; display N-W
      backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
      nwlogo   = comp_transparent_logo(nwimage, backgnd, 4, 555)
      tv, nwlogo, true=3, 4, 555
      ltot = tvrd(/true)
      erase
    endif

    ; plot doppler velocity
    restore, filepath('my_doppler_ct.sav', root=mg_src_root())
    tvlct, r, g, b
    vel = bytscl(velocity, min=-10, max=10, top=253)
    vel[unmasked] = 254
    if ((size(bad_val))[0] eq 1) then vel[bad_val] = 254
    tv, vel
    colorbar2, position=colbarpos, chars=1.25, title='LOS velocity [km/s]', $
               range=[-10, 10], font=-1, divisions=10, color=255, ncolors=253
    loadct, 0, /silent
    xyouts, 4 * 48, 4 * 97, 'Doppler', chars=6, /device, color=255
    xyouts, 4 * 48.5, 4 * 78, 'Velocity', chars=6, /device, color=255
    !p.font = -1
    xyouts, 4 * 1, 4 * 151.5, 'CoMP ' + wave_type, chars=1, /device, color=255
    xyouts, 4 * 109, 4 * 151.5, $
            index[ii].date_d$obs + ' ' + index[ii].time_d$obs + ' UT', $
            chars=1, /device, color=255
    !p.font = 1

    ; display HAO logo
    tvlct, rtemp, gtemp, btemp, /get
    tvlct, rhao, ghao, bhao
    tv, haologo
    tvlct, rtemp, gtemp, btemp

    ; display NSF/NCAR logo
    backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
    nsflogo   = comp_transparent_logo(nsfimage, backgnd, 619 - 134, 0)
    tv, nsflogo, true=3, 619-134, 0

    ; display N-W
    backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
    nwlogo   = comp_transparent_logo(nwimage, backgnd, 4, 555)
    tv, nwlogo, true=3, 4, 555
    corr_velo = tvrd(/true)
    erase

    ; plot intensity
    comp_aia_lct, wave=193, /load
    int = sqrt(intensity)
    int = bytscl(int, min=1, max=5)
    if ((size(bad_val))[0] eq 1) then int[bad_val] = 0
    tv, int
    colorbar2, position=colbarpos, chars=1.25, title='sqrt(intensity)', $
               range=[1, 5], font=-1, divisions=4
    loadct, 0, /silent
    xyouts, 4 * 48, 4 * 78, 'Intensity', chars=6, /device, color=255
    !p.font = -1
    xyouts, 4 * 1, 4 * 151.5, 'CoMP ' + wave_type, chars=1, /device, color=255
    xyouts, 4 * 109, 4 * 151.5, $
            index[ii].date_d$obs + ' ' + index[ii].time_d$obs + ' UT', $
            chars=1, /device, color=255
    !p.font = 1

    ; display HAO logo
    tvlct, rtemp, gtemp, btemp, /get
    tvlct, rhao, ghao, bhao
    tv, haologo
    tvlct, rtemp, gtemp, btemp

    ; display NSF/NCAR logo
    backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
    nsflogo   = comp_transparent_logo(nsfimage, backgnd, 619 - 134, 0)
    tv, nsflogo, TRUE=3, 619-134, 0

    ; display N-W
    backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
    nwlogo   = comp_transparent_logo(nwimage, backgnd, 4, 555)
    tv, nwlogo, true=3, 4, 555
    intensity = tvrd(/true)
    erase

    ; plot enhanced intensity
    comp_aia_lct, wave=193, /load
    tv, int_enh
    loadct, 0, /silent
    xyouts, 4 * 40, 4 * 85, 'Enhanced', chars=6, /device, color=255
    xyouts, 4 * 48, 4 * 68, 'Intensity', chars=6, /device, color=255
    !p.font = -1
    xyouts, 4 * 1, 4 * 151.5, 'CoMP ' + wave_type, chars=1, /device, color=255
    xyouts, 4 * 109, 4 * 151.5, $
            index[ii].date_d$obs + ' ' + index[ii].time_d$obs + ' UT', $
            chars=1, /device, color=255
    !p.font = 1

    ; display HAO logo
    tvlct, rtemp, gtemp, btemp, /get
    tvlct, rhao, ghao, bhao
    tv, haologo
    tvlct, rtemp, gtemp, btemp

    ; display NSF/NCAR logo
    backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
    nsflogo   = comp_transparent_logo(nsfimage, backgnd, 619 - 134, 0)
    tv, nsflogo, true=3, 619 - 134, 0

    ; display N-W
    backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
    nwlogo   = comp_transparent_logo(nwimage, backgnd, 4, 555)
    tv, nwlogo, true=3, 4, 555
    enhanced_intensity = tvrd(/true)
    erase

    ; plot line width
    loadct, 4, /silent
    tvlct, r, g, b, /get
    ;r=reverse(r) & g=reverse(g) & b=reverse(b)
    ;r[255]=255 & g[255]=255 & b[255]=255
    ;r[254]=0 & g[254]=0 & b[254]=0
    b[255] = 255
    tvlct, r, g, b
    ;width = bytscl(width, min=35, max=65, top=253)
    ;width[unmasked] = 254
    width = bytscl(width, min=25, max=55, top=254)
    if ((size(bad_val))[0] eq 1) then width[bad_val] = 0
    tv, width
    colorbar2, position=colbarpos,chars=1.25, title='line width [km/s]', $
               range=[25,55], font=-1, divisions=3, color=255, ncolors=254
    loadct, 0, /silent
    xyouts, 4 * 38, 4 * 78, 'Line Width', charsize=6, /device, color=255
    !p.font = -1
    xyouts, 4 * 1, 4 * 151.5, 'CoMP ' + wave_type, charsize=1, /device, color=255
    xyouts, 4 * 109, 4 * 151.5, $
            index[ii].date_d$obs + ' ' + index[ii].time_d$obs + ' UT', $
            charsize=1, /device, color=255
    !p.font = 1

    ; display HAO logo
    tvlct, rtemp, gtemp, btemp, /get
    tvlct, rhao, ghao, bhao
    tv, haologo
    tvlct, rtemp, gtemp, btemp

    ; display NSF/NCAR logo
    backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
    nsflogo   = comp_transparent_logo(nsfimage, backgnd, 619 - 134, 0)
    tv, nsflogo, true=3, 619-134, 0

    ; display N-W
    backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
    nwlogo   = comp_transparent_logo(nwimage, backgnd, 4, 555)
    tv, nwlogo, true=3, 4, 555
    line_width = tvrd(/true)
    erase

    ; plot azimuth
    if (qu_files[ii] eq 1) then begin
      p_angle = float(index[ii].solar_p0)
      azimuth = (0.5*atan(stks_u, stks_q) * 180. / !pi) - p_angle + 45.
      azimuth = azimuth mod 180.
      bad_az  = where(azimuth lt 0.)
      if ((size(bad_az))[0] eq 1) then azimuth[bad_az] += 180.
      loadct, 4, /silent
      tvlct, r, g, b, /get
      b[255] = 255
      tvlct, r, g, b
      azi = bytscl(azimuth, min=0, max=180, top=254)
      azi[unmasked] = 0.
      tv, azi
      colorbar2, position=colbarpos,chars=1.25, title='Azimuth [degrees]', $
                 range=[0, 180], font=-1, divisions=6, color=255, ncolors=254
      loadct, 0, /silent
      xyouts, 4 * 48, 4 * 78, 'Azimuth',chars=6, /device, color=255
      !p.font = -1
      xyouts, 4 * 1, 4 * 151.5, 'CoMP ' + wave_type, charsize=1, /device, color=255
      xyouts, 4 * 109, 4 * 151.5, $
              index[ii].date_d$obs + ' ' + index[ii].time_d$obs + ' UT', $
              charsize=1, /device, color=255
      !p.font = 1

      ; display HAO logo
      tvlct, rtemp, gtemp, btemp, /get
      tvlct, rhao, ghao, bhao
      tv, haologo
      tvlct, rtemp, gtemp, btemp

      ; display NSF/NCAR logo
      backgnd   = tvrd(619 - 134, 0, nsfimsize[0], nsfimsize[1], true=3)
      nsflogo   = comp_transparent_logo(nsfimage, backgnd, 619 - 134, 0)
      tv, nsflogo, true=3, 619 - 134, 0

      ; display N-W
      backgnd  = tvrd(4, 555, nwimsize[0], nwimsize[1], true=3)
      nwlogo   = comp_transparent_logo(nwimage, backgnd, 4, 555)
      tv, nwlogo, true=3, 4, 555
      azim = tvrd(/true)
      erase
    endif

    timestamp = strmid(index[ii].time_d$obs,0,2) $
                  + strmid(index[ii].time_d$obs, 3, 2) $
                  + strmid(index[ii].time_d$obs, 6, 2)

    if (ii + 1 ge 1000) then prefix = ''
    if (ii + 1 lt 1000) then prefix = '0'
    if (ii + 1 lt 100)  then prefix = '00'
    if (ii + 1 lt 10)   then prefix = '000'
    im_no = strcompress(string(ii + 1), /remove_all)
    png_ext = wave_type + '.' + nwlst + '.' + prefix + im_no + '.png'
    write_png, filepath('intensity.' + png_ext, root=temp_path), $
               intensity
    write_png, filepath('enhanced_intensity.' + png_ext, root=temp_path), $
               enhanced_intensity
    write_png, filepath('corrected_velocity.' + png_ext, root=temp_path), $
               corr_velo
    write_png, filepath('line_width.' + png_ext, root=temp_path), $
               line_width

    if (qu_files[ii] eq 1) then begin
      if (p_counter ge 1000) then pprefix = ''
      if (p_counter lt 1000) then pprefix = '0'
      if (p_counter lt 100)  then pprefix = '00'
      if (p_counter lt 10)   then pprefix = '000'
      png_ext = wave_type + '.' + nwlst + '.' + pprefix $
                  + strcompress(string(p_counter), /remove_all) + '.png'
      write_png, filepath('q.' + png_ext, root=temp_path), qoveri
      write_png, filepath('u.' + png_ext, root=temp_path), uoveri
      write_png, filepath('ltot.' + png_ext, root=temp_path), ltot
      if (nwlst eq '3') then begin
        write_png, filepath('azimuth.' + png_ext, root=temp_path), azim
      endif
    endif
  endfor

  ; set_plot, 'X'
  mg_log, 'PNG creation for movies finished. Making movies now...', $
          name='comp', /info
  cd, current=pwd
  cd, temp_path

  ffmpeg_fmt = '(%"ffmpeg -r 25 -i %s' $
                 + ' -y -pass %d' $
                 + ' -loglevel error' $
                 + ' -vcodec libx264' $
                 + ' -passlogfile %s' $
                 + ' -b:v %s -g 3' $
                 + ' %s.comp.%s.%s.%s.mp4")'
  infile_ext = '.' + wave_type + '.' + nwlst + '.%04d.png'

  ; 2-pass encoding with ffmpeg and x264
  for mmm = 1, 2 do begin
    spawn, filepath(string('intensity' + infile_ext, $
                           mmm, 'int', '2000k', $
                           date_dir, wave_type, 'daily_intensity', nwlst, $
                           format=ffmpeg_fmt), $
                    root=ffmpeg_dir)
    spawn, filepath(string('enhanced_intensity' + infile_ext, $
                           mmm, 'enh_int', '3000k', $
                           date_dir, wave_type, 'daily_enhanced_intensity', nwlst, $
                           format=ffmpeg_fmt), $
                    root=ffmpeg_dir)
    spawn, filepath(string('corrected_velocity' + infile_ext, $
                           mmm, 'corr_velo', '3000k', $
                           date_dir, wave_type, 'daily_corrected_velocity', nwlst, $
                           format=ffmpeg_fmt), $
                    root=ffmpeg_dir)
    spawn, filepath(string('line_width' + infile_ext, $
                           mmm, 'line_width', '3000k', $
                           date_dir, wave_type, 'daily_line_width', nwlst, $
                           format=ffmpeg_fmt), $
                    root=ffmpeg_dir)
    spawn, filepath(string('ltot' + infile_ext, $
                           mmm, 'lin_pol', '3000k', $
                           date_dir, wave_type, 'daily_ltot', nwlst, $
                           format=ffmpeg_fmt), $
                    root=ffmpeg_dir)
    spawn, filepath(string('q' + infile_ext, $
                           mmm, 'stks_q', '3000k', $
                           date_dir, wave_type, 'daily_q', nwlst, $
                           format=ffmpeg_fmt), $
                    root=ffmpeg_dir)
    spawn, filepath(string('u' + infile_ext, $
                           mmm, 'stks_u', '3000k', $
                           date_dir, wave_type, 'daily_u', nwlst, $
                           format=ffmpeg_fmt), $
                    root=ffmpeg_dir)

    if (nwlst eq '3') then begin
      spawn, filepath(string('azimuth' + infile_ext, $
                             mmm, 'azi', '3000k', $
                             date_dir, wave_type, 'daily_azimuth', nwlst, $
                             format=ffmpeg_fmt), $
                      root=ffmpeg_dir)
    endif
  endfor

  cd, pwd

  skip:
  mg_log, 'done', name='comp', /info
end
