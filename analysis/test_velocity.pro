; @detoma code for calculating velocity

pro test_velocity, int_th=int_th, $
                   true_rest=true_rest, $
                   file=file, $
                   th_lambda=th_lambda, $
                   nli=nli, $
                   usevc=usevc

  default, int_th, 0.1
  ; TODO: change to 0.3
  default, th_lambda, 0.4   ; this is to exclude shift larger than 0.4nm or velocity of 112km/s
  default, true_rest, 1074.62  ; 1074.605 
  default, file, '20171230.181348.comp.1074.iqu.5.fts.gz'
  default, nli, 10 
  default, usevc, 1 

  ; speed of light
  c = 299792.458D

  file = '/hao/sunset/Data/CoMP/process.uncorrected-velocity/20171230/level1/' + file
  file = '/hao/sunset/Data/CoMP/process.pseudostream/20130411/level1/20130411.174603.comp.1074.iqu.5.fts.gz'

  fits_open, file, fcb
  print, fcb.nextend
  print, file

  fit_arr_in = dblarr(620, 620, 3)
  image = readfits(file, hdu, ext=0)
  nll = sxpar(hdu, 'NTUNE') 

  if (nll eq 0) then nll = sxpar(hdu, 'NTUNES') 
  nx = 620
  ny = 620

  occx =  sxpar(hdu, 'CRPIX1') - 1
  occy =  sxpar(hdu, 'CRPIX2') - 1
  occr =  sxpar(hdu, 'ORADIUS')
  fccx =  sxpar(hdu, 'FRPIX1') - 1   ;  nx - sxpar (hdu, 'FRPIX1') 
  fccy =  sxpar(hdu, 'FRPIX2') - 1   ;  ny - sxpar (hdu, 'FRPIX2') 
  fccr =  sxpar(hdu, 'FRADIUS')
  mask = fltarr(nx, ny)
  mask[*, *] = 0
  xxc = findgen(nx, ny) mod nx - occx
  yyc = transpose(findgen(ny, nx) mod ny) - occy
  xxf = findgen(nx, ny) mod nx - fccx
  yyf = transpose(findgen(ny, nx) mod ny) - fccy
  rrc = sqrt(xxc^2. + yyc^2.)
  rrf = sqrt(xxf^2. + yyf^2.)
  pick = where(rrc gt occr + 4. and rrf lt fccr - 10.)
  mask[pick] = 1

  ; I AM DOING A ROUGH MASKING BECAUSE COMP_MAKE_MASK DID NOT COMPILE
  ; MIKE: YOU SHOULD USE THE MASKING ROUTINE YOU ALREADY HAVE
  ; PLAY WITH HOW MANY PIXELS TO STAY AWAY FROM THE OCCULTER AND OUTER FOV
  ; I USED 4 AND 10

  ;MIKE: YOU DO NO NEED TO DO THAT EITHER
  ;save data
  ; 3-points
  if (nll eq 3) then begin 
    intensity1 = readfits(file, hdu1, ext=1)
    intensity2 = readfits(file, hdu2, ext=2)
    intensity3 = readfits(file, hdu3, ext=3)
  endif
  ; 5-points 
  if (nll eq 5) then begin 
    intensity1 = readfits(file, hdu1, ext=2)
    intensity2 = readfits(file, hdu2, ext=3)
    intensity3 = readfits(file, hdu3, ext=4)
  endif


  ; remove very low intensity values or values that are too high
  ; TODO: use FINITE routine below
  bad = where(intensity1 lt 0.1 or intensity2 lt 0.01 or intensity3 lt 0.01 or $
              intensity1 ge 60 or intensity2 ge 60 or intensity3 ge 60 or $
              intensity1 eq !VALUES.F_NAN or intensity2 eq !VALUES.F_NAN or intensity3 eq !VALUES.F_NAN)
  mask[bad] = 0
  good = where(mask gt 0, ngood)


  window, 3, xsize=620, ysize=620, retain=2
  wset, 3
  loadct, 0
  tv, bytscl(sqrt(intensity2), 0, 3.5)
  loadct, 39
  ;draw_circle, occx, occy, occr, /dev, color=240, lines=2
  draw_circle, fccx, fccy, fccr, /dev, color=30, lines=2, thick=2
  draw_circle, occx, occy, occr, /dev, color=30, lines=2, thick=2
  ;draw_circle, fccx, fccy, fccr-10,/dev, color=50, lines=1
  ;draw_circle, occx, occy, occr+4, /dev, color=50, lines=1
  loadct, 0


  ; perform the gaussian fit
  fit_array_in = dblarr(nx, ny, 3)
  fit_array_in(*,*,*)=0.0
  wavel = [sxpar(hdu1, 'WAVELENG'), $
           sxpar(hdu2, 'WAVELENG'), $
           sxpar(hdu3, 'WAVELENG')]
  print, 'wavelength ', nll, wavel
  d_lambda = double(mean(deriv(wavel)))
  rest = wavel[1]
  counter = 0
  gaussx = findgen(30) * 0.01 + 1074.5

  for yy=0, ny-1 do begin
    for xx=0, nx-1 do begin
      if mask(xx, yy) eq 1  then begin
        profile=double([reform(intensity1(xx,yy)), reform(intensity2(xx,yy)), reform(intensity3(xx,yy))])
        if profile(1) gt int_th+1.0 and profile(0) gt 0 and profile(2) gt 0 then  begin
          ; uncomment to display gaussian fit
          ;          plot, wavel, profile, psym=2   
          comp_analytic_gauss_fit, profile, d_lambda, doppler_shift, width, i_cent
          ;            xyouts, 1074.55, 0.1, doppler_shift, size=2
          ;            gaussy = i_cent * exp (- (gaussx -  wavel(1) - doppler_shift)^2/ width^2)
          ;            oplot, gaussx, gaussy, lines=2 
          ;            oplot, [wavel(1) + doppler_shift, wavel(1) + doppler_shift], [0, i_cent], lines=1 & wait, 1
          ; do not consider profile if center intensity is too different from gaussian peak intensity
          if (abs(i_cent-profile[1]) gt 1.5*profile[1] ) then begin
            fit_array_in[xx,yy,0] = 0D
            fit_array_in[xx,yy,1] = 0D
            fit_array_in[xx,yy,2] = 0D
          endif else begin
            fit_array_in[xx,yy,0] = i_cent
            fit_array_in[xx,yy,1] = wavel(1) + doppler_shift
            fit_array_in[xx,yy,2] = width/sqrt(2.)
            counter =counter + 1.0
          endelse
        endif
      endif
    endfor
  endfor

  center_intensity = reform(fit_array_in[*, *, 0])
  center_lambda    = reform(fit_array_in[*, *, 1])

  ; plot peak wavelength distribution
  window, 2, xsize=620, ysize=620, retain=2
  wset, 2

  lpick = where(center_lambda gt 0, npick)
  print, ' min , max lambda    ', minmax(center_lambda[lpick])
  print, ' mean, median lambda ', $
         mean(center_lambda[lpick]), median(center_lambda[lpick])
  mlambda = median(center_lambda[lpick])
  plot,  findgen(100) * 0.001 + 1074.55, $
         histogram(center_lambda[lpick], binsize=0.001, min=1074.55), $
         psym=10, xstyle=1
  plots, [true_rest, true_rest], [0, 10000], lines=1
  plots, [1074.62, 1074.62], [0, 10000], lines=1
  plots, [mlambda, mlambda], [0, 10000], lines=2

  print, 'number of good profiles ', counter, counter * 100.0 / float(ngood), '%'
  print, 'rms of peak wavelength' , rms(center_lambda[lpick])
  stop

  ; save original rest wavelength
  original_rest = true_rest

  ; redifine true_rest
  if (mlambda - true_rest) lt 0.05  then true_rest = mlambda


  ; east limb
  ; TODO: center_intensity gt 1.0
  blue_good = where(mask gt 0 and center_intensity ge int_th+1. and center_lambda gt 0 and abs(center_lambda - true_rest) lt th_lambda and xxc lt 0, nblue)

  ; west limb
  red_good  = where(mask gt 0 and center_intensity ge int_th+1. and center_lambda gt 0 and abs(center_lambda - true_rest) lt th_lambda and xxc gt 0, nred)
  print, 'red and blue good point ' , nred, nblue
  print, 'mean red and blue wavelength ', mean(center_lambda(red_good)), mean(center_lambda(blue_good))

  ; TODO: produce mean of E and W medians and compare to rest wavelength from
  ; continuum correction; warn if difference bigger 0.02

  all_good = where(mask gt 0 and center_intensity ge int_th+1. and center_lambda gt 0 and abs(center_lambda - true_rest) lt th_lambda, nall)


  ;true_rest =  (median(center_lambda(red_good)) + median(center_lambda(blue_good)))/2.
  mask2 = fltarr(nx, yy)
  good = where(mask gt 0 and center_intensity ge int_th + 0.5 and center_lambda gt 0 and abs(center_lambda - true_rest) lt th_lambda, n_good)

  print, ' average blue/red  , mean all good ', $
         (mean(center_lambda(red_good)) + mean(center_lambda(blue_good)))/2., $
         mean(center_lambda(all_good)),  mean(center_lambda(good))


  window, 4, xsize=620, ysize=620, retain=2
  loadct, 0
  wset, 4

  ; plot the mask
  high = where(mask gt 0 and abs(center_lambda - true_rest) ge th_lambda, n_high)
  print, 'point with high velocity ', n_high
  mask2[good] = 1
  mask2[all_good] = 2
  tvscl, mask2
  xyouts, 0.1, 0.1, 'mask', /norm


  wset, 2
  new_rest = (median(center_lambda[red_good]) + median(center_lambda[blue_good]))/2.
  plots, [new_rest, new_rest], [0, 10000], lines=2, thick=2
  print, ' difference in median velocity ',  (mlambda - new_rest) * c / double(new_rest) 

  true_rest = new_rest ; this used the newly derived rest wavelength

  ;if usevc set to zero use original rest wavelength

  if (usevc) eq 0 then true_rest = original_rest ; original code 

  stop


  ;MIKE: HERE STARTS THE 5-ORDER FIT THA THUI WROTE
  x = dindgen(nx) - nx / 2   ; in pix for plotting
  ewtrend = dblarr(nx)
  residualtrend = dblarr(nx)
  residualtrend[*] = true_rest
  counter = 0
  i = 0
  vpoints = intarr(nx)

  for i=0, nx-1 do begin
    ;exclude pixels with no data or low S/N data and also bad velocity data
    ;sub_good =where(center_intensity(i,*) ge 2 and center_lambda(i,*) gt 0,
    ;n_subgood)
    ; TODO: don't need to do below line
    sub_good = where(center_intensity(i,*) ge int_th+1.0 and center_lambda(i,*) gt 0 $
                     and abs(center_lambda(i,*)-true_rest) lt 0.24, n_subgood) ;exclude high velocities so you do not skew the fit
    ; original code uses ne 10 
    ; MIKE: YOU CAN PLAY WITH THIS NUMBER 
    ; NOT SURE WHICH VALUS GIVES LESS SIDE EFFECTS
    if (n_subgood ge nli) then begin
      residualtrend[i] = median(center_lambda[i,sub_good]) 
      counter = counter + 1
      vpoints[i] = n_subgood
    endif
    ; print bad columns
    ;    if n_subgood gt 0 and n_subgood le 30 then print, i, x(i), n_subgood, median(center_lambda[i,sub_good])
    ;    if n_subgood eq 0 then print, i, x(i), n_subgood
  endfor
  ; PLOT FITS
  window, 1, retain=2
  wset, 1
  !p.multi = [0, 1, 2]
  plot, x, vpoints, ystyle=1, psym=10, xstyle=1, $
        ytitle='number of pixels per vertical slice'
  plot, x, residualtrend, title=title, ystyle=1, xstyle=1, charsize=1.2, $
        xtitle='pixel x-position', ytitle= 'median wavelength'
  plots, [-310, 310], [true_rest, true_rest], lines=2

  print, 'number of median values computed for each x ', counter

  sub_abnor = where(abs(residualtrend - true_rest) ge 0.4, nabnor)
  if sub_abnor[0] ne -1 then begin
     residualtrend[sub_abnor]=true_rest      ; this is where I believe causes things to go crazy under the occulter if you use 1074.62
  print, 'WARNING: anomalous velocity', nabnor
  endif
  print, 'number of abnormal vel ', nabnor

stop
 
 
  sub_eff=where(residualtrend ne true_rest, n_eff)
  s1=sub_eff[0]
  s2=sub_eff[n_eff-1]
   
  loadct,39
  oplot, x(sub_eff), residualtrend(sub_eff), psym=2, color=100
  ;interpolate over missing data points
  ;residualtrend = interpol(residualtrend(sub_eff), x(sub_eff), x)
  residualtrend(0:s1)=residualtrend(s1+1)
  residualtrend(s2:nx-1)=residualtrend(s2-1)
  ;median filter of the east-west trend
  residualtrend=fmedian(residualtrend,20)
  loadct,39
  !p.multi=[0,1,1]
  oplot, x, residualtrend, thick=2, color=50
 
 
 
stop


  if (size(x[s1:s2]))[1] gt 1 then begin
    r=poly_fit(x[s1:s2],residualtrend[s1:s2],5,/double) ;5th order polynomial fit to the east-west trend
    ;r2=poly_fit(x(sub_eff),residualtrend(sub_eff),5,/double)
    ewtrend=poly(x,r)
    oplot, x, ewtrend, color=250, thick=2
    ;ewtrend2=poly(x,r2) 
     ewtrend(0:s1)=ewtrend(s1+1)
     ewtrend(s2:nx-1)=ewtrend(s2-1)
     oplot, x, ewtrend, color=250, lines=2, thick=2
    loadct,0
    resitren=reform(ewtrend)#(dblarr(ny)+1)
    ;eliminate the east-west trend
    center_lambda_corr = dblarr(nx,ny)
    center_lambda_corr(lpick) = center_lambda(lpick) - resitren(lpick)
  endif
; sub=where(reform(fit_arr_in[*,*,0]) ge int_th+1.0) ;exclude pixels with no data or low S/N data
  good =where(center_intensity ge int_th+1.0 and center_lambda gt 0 $
                    and abs(center_lambda-true_rest) lt 0.36, n_good)
  if n_good gt 0 then begin
     temptrend = median(center_lambda_corr(good)) 
  endif else begin
    temptrend = 0
endelse



; velocity corrected by solar rotation
velocity2 = dblarr(nx,ny)
pick=where(center_lambda gt 0 and mask gt 0)
velocity2(pick) = (center_lambda_corr(pick) - temptrend)*c/double(true_rest)

; veocity not corrected for solar rotation - using new rest wavelength
; TODO: use the following to create uncorrected velocity in comp_l2_analytical
velocity_new = (center_lambda - new_rest)*c/new_rest
bad=where(center_intensity lt int_th+1. and center_lambda eq 0)
velocity_new(bad)=0

; veocity not corrected for solar rotation - using original rest wavelength
velocity = (center_lambda - original_rest)*c/original_rest
bad=where(center_intensity lt int_th+1. and center_lambda eq 0)
velocity(bad)=0

stop


window, 1, xsize=620*3, ysize=620, retain=2 & wset,1
wset,1

restore, 'my_doppler_ct.sav'
    tvlct, r, g, b
tv, bytscl(velocity2, -10,10)
xyouts, 0.03, 0.01, 'rotation corrected' + string(true_rest), /norm, color=0, size=2
tv, bytscl(velocity, -10,10), 0.3333, 0, /norm
xyouts, 0.36, 0.01, 'not corrected - rest wavelength' + string(original_rest), /norm, color=0, size=2
tv, bytscl(velocity_new, -10,10), 0.666, 0, /norm
xyouts, 0.69, 0.01, 'not corrected - rest wavelength' + string(new_rest), /norm, color=0, size=2
print, 'nominal, true rest, and median rest lambda ', rest, true_rest, temptrend


intensity=fit_array_in(*,*,0)
pick =where(intensity gt 0)
print, ' median intensity value ', median(intensity(pick))


stop

end
