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

  dir = filepath('', subdir=[date_dir, 'level2'], root=process_basedir)
  filename = string(date_dir, wave_type, format='(%"%s.comp.%s.median.fts.gz")')

  if (keyword_set(debug)) then begin
    window, 0, xsize=2 * 500, ysize=500
    window, 1, xsize=4 * nx, ysize=nx, title='Stokes I, Q, U, and V'
    window, 2, xsize=5 * nx, ysize=nx, title='V crosstalk'
  endif

  device, decomposed=0
  loadct, 0
  ;openw,1,dir+'crosstalk.log'

  ; read data
  if (keyword_set(debug)) then print, 'reading data'

  fits_open, filepath(filename, root=dir), fcb
  num = fcb.nextend

  fits_read, fcb, data, p_header, /header_only, exten_no=0
  ntune = sxpar(p_header, 'NTUNES')
  ; compute center wavelength index (0 is first)
  nc = fix(ntune / 2)                 

  ; create arrays
  stokes_i   = fltarr(nx, nx, ntune)
  stokes_q   = fltarr(nx, nx, ntune)
  stokes_u   = fltarr(nx, nx, ntune)
  stokes_v   = fltarr(nx, nx, ntune)
  background = fltarr(nx, nx, ntune)

  ; create mask
  comp_make_mask2, p_header, mask, occ_fac=1.06, fld_fac=0.98

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
    fits_read, fcb, data, header, exten_no=i+1+3*ntune
    stokes_v[*, *, i] = data * mask
    ; read background
    fits_read, fcb, data, header, exten_no=i + 1 + 4 * ntune
    background[*, *, i] = data * mask
  endfor

  fits_close, fcb

  ; display stokes images at center wavelength
  if (keyword_set(debug)) then begin
    wset, 1
    !p.multi = [0, 4, 1, 0, 0]
    tv, bytscl(stokes_i[*, *, nc] * mask, 0, 25), 0
    tv, bytscl(stokes_q[*, *, nc] * mask, -0.8, 0.8), 1
    tv, bytscl(stokes_u[*, *, nc] * mask, -0.8, 0.8), 2
    tv, bytscl(stokes_v[*, *, nc] * mask, -0.15, 0.15), 3
  endif

  ; determine bright and faint intensity pixels
  corona = stokes_i[*, *, nc]
  faint = where(mask eq 1.0 and corona gt 0.0 and corona lt 3.0)

  ; add background back into stokes_i
  for i = 0, ntune - 1 do stokes_i[*, *, i] = stokes_i[*, *, i] + background[*, *, i]

  ; determine stokes_i to q and u crosstalk (use continuum wavelength index 0,
  ; and faint pixels)

  if (keyword_set(debug)) then begin
    print, 'computing I to Q, U crosstalk'

    wset, 0
    !p.multi = [0, 2, 1, 0, 0]
  endif

  i_cont = stokes_i[*, *, 0]
  x = i_cont[faint]
  q_cont = stokes_q[*, *, 0]
  y = q_cont[faint]
  i_to_q = median(y / x)

  if (keyword_set(debug)) then begin
    xfit = findgen(100)
    yfit = i_to_q * xfit

    i_to_q_rms = sqrt(mean((x * i_to_q - y)^2))
    print, i_to_q_rms, format='(%"i_to_q_rms      : %f")'
    plot, x, y, psym=3, xtitle='Stokes I', ytitle='Stokes Q', charsize=1.5
    oplot, xfit, yfit
  endif

  print, i_to_q, format='(%"i_to_q_xtalk    : %f")'

  u_cont = stokes_u[*, *, 0]
  y = u_cont[faint]
  i_to_u = median(y / x)

  if (keyword_set(debug)) then begin
    yfit = i_to_u * xfit

    i_to_u_rms = sqrt(mean((x * i_to_u - y)^2))
    print, i_to_u_rms, format='(%"i_to_u_rms      : %f")'
    plot, x, y, psym=3, xtitle = 'Stokes I', ytitle='Stokes U', charsize=1.5
    oplot, xfit, yfit
  endif

  print, i_to_u, format='(%"i_to_u_xtalk    : %f")'

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

  print, p[0], format='(%"i_to_v_xtalk    : %f")'
  print, p[1], format='(%"q_to_v_xtalk    : %f")'
  print, p[2], format='(%"u_to_v_xtalk    : %f")'

  if (keyword_set(debug)) then begin
    print, fmin, format='(%"fmin            : %f")'

    ; evaluate stokes V rms
    v_center = stokes_v[*, *, nc]
    print, stdev(v_center[where(mask eq 1.)]), format='(%"V rms before    : %d")'
    print, max(abs(stokes_v)), format='(%"Max(V) before   : %f")'

    ; apply crosstalk correction to stokes V
    for i = 0, ntune - 1 do begin
      stokes_v[*, *, i] = stokes_v[*, *, i] $
                            - p[0] * stokes_i[*, *, i] $
                            - p[1] * stokes_q[*, *, i] $
                            - p[2] * stokes_u[*, *, i]
    endfor

    wset, 2
    !p.multi = [0, 5, 1, 0, 0]
    tv, bytscl(stokes_v[*, *, 0] * mask, -0.1, 0.1), 0
    tv, bytscl(stokes_v[*, *, 1] * mask, -0.1, 0.1), 1
    tv, bytscl(stokes_v[*, *, 2] * mask, -0.1, 0.1), 2
    tv, bytscl(stokes_v[*, *, 3] * mask, -0.1, 0.1), 3
    tv, bytscl(stokes_v[*, *, 4] * mask, -0.1, 0.1), 4

    tmp = stokes_i[*, *, 0]
    low_ind = where(tmp[faint] lt 11.0, count)
    low_xy = array_indices([620, 620], faint[low_ind], /dimensions)

    device, decomposed=1
    plots, low_xy[0, *], low_xy[1, *], /device, color='0000ff'x, psym=3
    device, decomposed=0

    v_center = stokes_v[*, *, nc]
    print, stdev(v_center[where(mask eq 1.0)]), format='(%"V rms after     : %f")'
    print, max(abs(stokes_v)), format='(%"Max(V) after    : %f")'
  endif
end


; main-level example program

process_basedir = '/hao/compdata1/Data/CoMP/process.crosstalk'
date_dirs = ['20160725', '20160801', '20160802', '20160803']

for d = 0L, n_elements(date_dirs) - 1L do begin
  comp_initialize, date_dirs[d]
  print, date_dirs[d], format='(%"[%s]")'
  comp_crosstalk, process_basedir, date_dirs[d], /debug
  if (d lt n_elements(date_dirs) - 1L) then print
endfor

end
