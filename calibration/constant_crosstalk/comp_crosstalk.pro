; docformat = 'rst'

;+
; Procedure to compute crosstalk in comp stokes parameters.
;
; :Keywords:
;   debug : in, optional, type=boolean
;     set to display plots and more information about fit
;-
pro comp_crosstalk, process_basedir, date_dir, debug=debug
  compile_opt strictarr
  common fit, stokes_i, stokes_q, stokes_u, stokes_v

  ans = ' '
  nx = 620

  wave_type = '1074'

  dir = filepath('level2', subdir=date_dir, root=process_basedir)
  filename = string(date_dir, wave_type, format='(%"%s.comp.%s.mean.fts.gz")')

  if (keyword_set(debug)) then begin
    window, xsize=2 * 500, ysize=500, /free, $
            title=string(date_dir, format='(%"Foreground plots (%s)")')
    main_window = !d.window
    window, xsize=2 * 500, ysize=500, /free, $
            title=string(date_dir, format='(%"Background plots (%s)")')
    back_window = !d.window

    window, xsize=2 * 500, ysize=500, /free, $
            title=string(date_dir, format='(%"Faint I histogram (%s)")')
    hist_window = !d.window

    window, xsize=4 * nx, ysize=nx, /free, $
            title=string(date_dir, format='(%"Stokes I, Q, U, and V (%s)")')
    iquv_window = !d.window
    window, xsize=5 * nx, ysize=nx, /free, $
            title=string(date_dir, format='(%"V crosstalk (%s)")')
    v_window = !d.window
  endif

  device, decomposed=0
  loadct, 0

  ; read data
  fits_open, filepath(filename, root=dir), fcb
  num = fcb.nextend

  fits_read, fcb, data, p_header, /header_only, exten_no=0
  ntune = sxpar(p_header, 'NTUNES')

  ; compute center wavelength index (0 is first)
  nc = fix(ntune / 2)

  ; create arrays
  stokes_i     = fltarr(nx, nx, ntune)
  stokes_q     = fltarr(nx, nx, ntune)
  stokes_u     = fltarr(nx, nx, ntune)
  stokes_v     = fltarr(nx, nx, ntune)
  background_i = fltarr(nx, nx, ntune)
  background_q = fltarr(nx, nx, ntune)
  background_u = fltarr(nx, nx, ntune)
  background_v = fltarr(nx, nx, ntune)

  ; create mask
  erode_s = bytarr(25, 25) + 1B
  comp_make_mask2, p_header, mask, occ_fac=1.06, fld_fac=0.98
  mask = erode(mask, erode_s)

  for i = 0, ntune - 1 do begin
    ; read stokes I
    fits_read, fcb, data, header, exten_no=i + 1
    stokes_i[*, *, i] = data * mask

    ; read stokes Q
    fits_read, fcb, data, header, exten_no=i + 1 + ntune
    stokes_q[*, *, i] = data * mask

    ; read stokes U
    fits_read, fcb, data, header, exten_no=i + 1 + 2 * ntune
    stokes_u[*, *, i] = data * mask

    ; read stokes V
    fits_read, fcb, data, header, exten_no=i + 1 + 3 * ntune
    stokes_v[*, *, i] = data * mask

    ; read background I
    fits_read, fcb, data, header, exten_no=i + 1 + 4 * ntune
    background_i[*, *, i] = data * mask

    ; read background Q
    fits_read, fcb, data, header, exten_no=i + 1 + 5 * ntune
    background_q[*, *, i] = data * mask

    ; read background U
    fits_read, fcb, data, header, exten_no=i + 1 + 6 * ntune
    background_u[*, *, i] = data * mask

    ; read background V
    fits_read, fcb, data, header, exten_no=i + 1 + 7 * ntune
    background_v[*, *, i] = data * mask
  endfor

  fits_close, fcb

  ; display stokes images at center wavelength
  if (keyword_set(debug)) then begin
    wset, iquv_window

    tv, bytscl(stokes_i[*, *, nc] * mask,  0.0,  25.0),  0
    tv, bytscl(stokes_q[*, *, nc] * mask, -0.8,   0.8),  1
    tv, bytscl(stokes_u[*, *, nc] * mask, -0.8,   0.8),  2
    tv, bytscl(stokes_v[*, *, nc] * mask, -0.15,  0.15), 3
  endif

  ; determine bright and faint intensity pixels
  corona = stokes_i[*, *, nc]
  blue_wing = stokes_i[*, *, 0]
  faint = where(mask eq 1.0 and corona lt 5.0, $
                n_faint)

  ; add background back into stokes_i
  for i = 0, ntune - 1 do stokes_i[*, *, i] = stokes_i[*, *, i] + background_i[*, *, i]

  if (keyword_set(debug)) then begin
    masked = where(mask eq 1.0, n_masked)
    wset, hist_window
    !p.multi = 0
    h = histogram((stokes_i[*, *, 2])[masked], nbins=100, loc=loc)
    plot, loc, h, psym=10
  endif

  ; determine stokes_i to q and u crosstalk (use continuum wavelength index 0,
  ; and faint pixels)

  i_cont = stokes_i[*, *, 0]
  fg_i = i_cont[faint]
  q_cont = stokes_q[*, *, 0]
  fg_q = q_cont[faint]
  i_to_q = median(fg_q / fg_i)

  i_back = background_i[*, *, nc]
  bg_i = i_back[faint]
  q_back = background_q[*, *, nc]
  bg_q = q_back[faint]

  i_to_q_background = median(bg_q / bg_i)

  format = '(%"%-30s :", F15)'

  print, 'number of pts used', n_faint, format='(%"%-30s :", I15)'

  if (keyword_set(debug)) then begin
    xfit = findgen(100)
    yfit = i_to_q * xfit

    i_to_q_rms = sqrt(mean((fg_i * i_to_q - fg_q)^2))
    print, 'I to Q RMS', i_to_q_rms, format=format

    wset, main_window
    !p.multi = [0, 2, 1, 0, 0]
    plot, fg_i, fg_q, psym=3, xtitle='Stokes I', ytitle='Stokes Q', charsize=1.5
    oplot, xfit, yfit

    yfit = i_to_q_background * xfit

    wset, back_window
    !p.multi = [0, 2, 1, 0, 0]
    plot, bg_i, bg_q, $
          psym=3, xtitle='Stokes I', ytitle='Stokes Q', charsize=1.5
    oplot, xfit, yfit
  endif

  print, 'I to Q crosstalk', i_to_q, format=format
  print, 'I to Q crosstalk (background)', i_to_q_background, format=format

  u_cont = stokes_u[*, *, 0]
  fg_u = u_cont[faint]
  i_to_u = median(fg_u / fg_i)

  u_back = background_u[*, *, nc]
  bg_u = u_back[faint]

  i_to_u_background = median(bg_u / bg_i)

  if (keyword_set(debug)) then begin
    yfit = i_to_u * xfit

    i_to_u_rms = sqrt(mean((fg_i * i_to_u - fg_u)^2))
    print, 'I to U RMS', i_to_u_rms, format=format

    wset, main_window
    !p.multi = [1, 2, 1, 0, 0]
    plot, fg_i, fg_u, psym=3, xtitle = 'Stokes I', ytitle='Stokes U', charsize=1.5
    oplot, xfit, yfit

    yfit = i_to_u_background * xfit

    wset, back_window
    !p.multi = [1, 2, 1, 0, 0]
    plot, bg_i, bg_u, $
          psym=3, xtitle='Stokes I', ytitle='Stokes U', charsize=1.5
    oplot, xfit, yfit
  endif

  print, 'I to U crosstalk', i_to_u, format=format
  print, 'I to U crosstalk (background)', i_to_u_background, format=format

  ; correct stokes q and u for crosstalk from stokes I
  for i = 0, ntune - 1 do begin
    stokes_q[*, *, i] = stokes_q[*, *, i] - i_to_q * stokes_i[*, *, i]
    stokes_u[*, *, i] = stokes_u[*, *, i] - i_to_u * stokes_i[*, *, i]
  endfor

  ; fit stokes I, Q, and U to stokes V crosstalk using POWELL
  ftol = 1.e-4
  p = [0.0, 0.0, 0.0]   ; starting coefficients
  n = n_elements(p)
  xi = fltarr(n, n)
  for i = 0, n - 1 do xi[i, i] = 1.0
  xi = transpose(xi)   ; direction vector
  powell, p, xi, ftol, fmin, 'comp_cross_min'

  print, 'I to V crosstalk', p[0], format=format
  print, 'Q to V crosstalk', p[1], format=format
  print, 'U to V crosstalk', p[2], format=format

  if (keyword_set(debug)) then begin
    print, 'fmin', fmin, format=format

    ; evaluate stokes V rms
    v_center = stokes_v[*, *, nc]
    print, 'V RMS before', stdev(v_center[where(mask eq 1.)]), format=format
    print, 'max(V) before', max(abs(stokes_v)), format=format

    ; apply crosstalk correction to stokes V
    for i = 0, ntune - 1 do begin
      stokes_v[*, *, i] = stokes_v[*, *, i] $
                            - p[0] * stokes_i[*, *, i] $
                            - p[1] * stokes_q[*, *, i] $
                            - p[2] * stokes_u[*, *, i]
    endfor

    wset, v_window
    !p.multi = [0, 5, 1, 0, 0]
    tv, bytscl(stokes_v[*, *, 0] * mask, -0.1, 0.1), 0
    tv, bytscl(stokes_v[*, *, 1] * mask, -0.1, 0.1), 1
    tv, bytscl(stokes_v[*, *, 2] * mask, -0.1, 0.1), 2
    tv, bytscl(stokes_v[*, *, 3] * mask, -0.1, 0.1), 3
    tv, bytscl(stokes_v[*, *, 4] * mask, -0.1, 0.1), 4

    tmp = stokes_i[*, *, 0]
    low_ind = where(tmp[faint] lt 11.0, count)
    low_xy = array_indices([620, 620], faint[low_ind], /dimensions)
    print, 'number < 11.0', count, format='(%"%-30s :", I15)'

    device, decomposed=1
    plots, low_xy[0, *], low_xy[1, *], /device, color='0000ff'x, psym=3
    device, decomposed=0

    v_center = stokes_v[*, *, nc]
    print, 'V RMS after', stdev(v_center[where(mask eq 1.0)]), format=format
    print, 'max(V) after', max(abs(stokes_v)), format=format
  endif
end


; main-level example program

!except = 0
!quiet = 1

process_basedir = '/hao/mahidata1/Data/CoMP/process.empxtalk2'
;date_dirs = ['20160519', '20160609', '20160726']
date_dirs = ['20160519', '20160609']

for d = 0L, n_elements(date_dirs) - 1L do begin
  comp_initialize, date_dirs[d]
  print, date_dirs[d], format='(%"[%s]")'
  comp_crosstalk, process_basedir, date_dirs[d], /debug
  if (d lt n_elements(date_dirs) - 1L) then print
endfor

end
