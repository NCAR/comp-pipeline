;+
;  :Name: calibrate_comp_wavelength_2
;  
;  :Description: procedure to calibrate the wavelength scale of the comp instrument by 
;  fitting the intensity vs. wavelength in 11 point flat files to a reference spectrum.
;
;  :Input: 
;     date_dir
;     lam0 - central wavelength of region to fit (1074.7 or 1079.8 nm)
;
;  :Output:
;     offset - wavelength offset (nm) True Wavelength = CoMP Wavelength + Offset
;     h2o - h2o factor
;     flat_time - time flat was taken
;-
pro calibrate_comp_wavelength_2,date_dir,lam0,offset,h2o,flat_time,off_tell,dev=dev

common fit, wav, lambda, solar_spec, telluric_spec, filter_trans_on, filter_trans_off, obs, back

@comp_constants_common
@comp_mask_constants_common

COMPILE_OPT IDL2

; Configure
comp_initialize, date_dir
comp_paths, date_dir

if keyword_set(dev) eq 0 then dev='p'   ;if device not set, then output to ps

debug='no'     ;debug mode, 'yes' or 'no'
ans=' '

dir='C:\Users\tomczyk\Documents\Comp\idl\Systematics\'
cd,dir

if dev ne 'p' then begin
  window,0,xs=900,ys=1000
endif

;  open flat file for this day

file='V:\CoMP\process\'+date_dir+'\flat.fts'
r=file_search(file)
if r eq '' then file='V:\CoMP\process\'+date_dir+'\level1\flat.fts'

fits_open,file,fcb       ;open input file
num=fcb.nextend       ;get number of extensions


;  define masks for each beam

fits_read,fcb,d,flat_header,exten_no=1,/header_only     ;get header information

mask1=comp_mask_1024_1(flat_header)
mask2=comp_mask_1024_2(flat_header)
good1=where(mask1 eq 1.)
good2=where(mask2 eq 1.)

;  find 11 wavelength flats in flat file

nwave=11        ;use only 11 wavelength data

fits_read,fcb,waves,header,exten_no=num-1     ;read times for flats
fits_read,fcb,times,header,exten_no=num-2     ;read times for flats
u_time=times[uniq(times,sort(times))]         ;identify unique observation times
if debug eq 'yes' then print,u_time
nflat=0
f_index=intarr(10)      ;array to hold extension index of first flat in sequence
for i=0,n_elements(u_time)-1 do begin
  use=where(times eq u_time[i],count)
  if count eq 22 and abs(lam0-mean(abs(waves[use]))) lt 2. then begin    ;just look for 11 wavelength flats near target wavelength
    f_index[nflat]=use[0]+1
    nflat=nflat+1
  endif
endfor
f_index=f_index[0:nflat-1]
if debug eq 'yes' then print,'f_index:',f_index
print,nflat,' 11 wavelength flats at:',lam0 

if nflat gt 0 then begin

;  get solar and telluric spectra in this region from atlas
  
get_spectrum_solar_telluric,lam0,lambda,solar_spec,telluric_spec
nlambda=n_elements(lambda)
dlam=lambda[1]-lambda[0]      ;wavelength spacing of spectra

solar_spec=solar_spec/max(solar_spec)
telluric_spec=telluric_spec/max(telluric_spec)
  
offset=fltarr(nflat,2)      ;create arrays to hold results
h2o=fltarr(nflat,2)
off_tell=fltarr(nflat,2)
flat_time=times[f_index]


;------------------------loop over nflats flats

for iflat=0,nflat-1 do begin

  wav=dblarr(nwave)
  pol=strarr(nwave)
  obs1=dblarr(nwave)
  obs2=dblarr(nwave)
  back1=dblarr(nwave)
  back2=dblarr(nwave)

  for i=0,nwave-1 do begin      ;read in and average observations over annulus
  	fits_read,fcb,d1,header,exten_no=i+f_index[iflat]     ;beam 1
  	wav[i]=double(sxpar(header,'WAVELENG'))
  	pol[i]=double(sxpar(header,'POLSTATE'))
    datetime=sxpar(header,'FILENAME')
    
    obs1[i]=median(d1[good1])
    back2[i]=median(d1[good2])
    
    fits_read,fcb,d2,header,exten_no=2*nwave-i+f_index[iflat]-1    ;beam 2
    obs2[i]=median(d2[good2])
    back1[i]=median(d2[good1])
  endfor

;  put data in order of increasing wavelength

  s=sort(wav)
  wav=wav[s]
  obs1=obs1[s]
  obs2=obs2[s]
  back1=back1[s]
  back2=back2[s]
  pol=pol[s]

;  fit continuum in order to detrend spectrum

  w=wav-lam0
  
;  if lam0 eq 1074.7 then to_fit_obs=[0,1,6,10] else to_fit_obs=[1,2,4,8,10]      ;define continuum wavelength points
  if lam0 eq 1074.7 then to_fit_obs=[0,1,6,10] else to_fit_obs=[0,2,4,8,10]      ;define continuum wavelength points
  
  c=poly_fit(w[to_fit_obs],obs1[to_fit_obs],2,chisq=chisq)      ;fit observations with a 2nd order polynomial
  ofit1=poly(w,c)
  
  c=poly_fit(w[to_fit_obs],obs2[to_fit_obs],2,chisq=chisq)      ;fit observations with a 2nd order polynomial
  ofit2=poly(w,c)
  
  if lam0 eq 1074.7 then to_fit_back=[0,5,7,10] else to_fit_back=[0,3,6,9]       ;fit background continuum
  c=poly_fit(w[to_fit_back],back1[to_fit_back],2,chisq=chisq)      ;fit observations with a 2nd order polynomial
  bfit1=poly(w,c)
  
  c=poly_fit(w[to_fit_back],back2[to_fit_back],2,chisq=chisq)      ;fit observations with a 2nd order polynomial
  bfit2=poly(w,c)

;  plot data and fit to continuum

  if dev ne 'p' then wset,0
  !p.multi=[0,2,3,0,0]
  
  plot,wav,obs1,psym=2,tit='Observations and Continuum Fit',xtit='CoMP Wavelength (nm)',$
    ytit='Intensity (ppm)',yr=[0.,1.1*(max(obs1)>max(obs2))],chars=1.5
  oplot,wav,ofit1
  oplot,wav,obs2,psym=4
  oplot,wav,ofit2,linesty=1
  oplot,wav[to_fit_obs],obs1[to_fit_obs],psym=5
  oplot,wav[to_fit_obs],obs2[to_fit_obs],psym=5

  plot,wav,back1,psym=2,tit='Background and Continuum Fit',xtit='CoMP Wavelength (nm)',$
    ytit='Intensity (ppm)',yr=[0.,1.1*(max(obs1)>max(obs2))],chars=1.5
  oplot,wav,bfit1
  oplot,wav,back2,psym=4
  oplot,wav,bfit2,linesty=1
  oplot,wav[to_fit_back],back1[to_fit_back],psym=5
  oplot,wav[to_fit_back],back2[to_fit_back],psym=5

  xyouts,0.1,0.72,datetime,/norm,chars=1.
  if debug eq 'yes' then read,'enter return',ans
  
  obs1/=ofit1      ;divide data by fit
  obs2/=ofit2    
  back1/=bfit1   
  back2/=bfit2     

;  get comp transmission profiles
  
  filter_trans_on=fltarr(nlambda,nwave)
  filter_trans_off=fltarr(nlambda,nwave)
  for ii=0,nwave-1 do begin
    comp_trans,wav[ii],lambda,trans_on,trans_off   ;get comp transmission profiles
    trans_on=trans_on/total(trans_on)   ;normalize area under filters to 1
    trans_off=trans_off/total(trans_off)
    filter_trans_on[*,ii]=trans_on
    filter_trans_off[*,ii]=trans_off
  endfor

  
;  fit wavelength offset ,continuum factor and h2o strength with powell minimization
;  h2o strength is in units of nominal strength (1 = nominal, 0 = continuum)  

  ftol=1.d-8

  nparam=4      ;either 4 or 5
  if nparam eq 4 and lam0 eq 1074.7 then begin
    p1=[0.036d0,0.84d0,1.0d0,1.0d0]
  endif
  if nparam eq 5 and lam0 eq 1074.7 then begin
    p1=[0.036d0,0.84d0,1.0d0,1.0d0,0.036d0]
  endif
  if nparam eq 4 and lam0 eq 1079.8 then begin
    p1=[0.036d0,0.4d0,1.0d0,1.0d0]
  endif
  if nparam eq 5 and lam0 eq 1079.8 then begin
    p1=[0.036d0,0.4d0,1.0d0,1.0d0,0.1d0]
  endif

  p2=p1
    
  xi=dblarr(nparam,nparam)
  for i=0,nparam-1 do xi[i,i]=1.d0
  obs=obs1
  back=back1
  
  powell,p1,xi,ftol,fmin,'powfunc',/double
  p1[1]=abs(p1[1])
  print,p1,fmin

  xi=dblarr(nparam,nparam)
  for i=0,nparam-1 do xi[i,i]=1.d0
  obs=obs2
  back=back2
  powell,p2,xi,ftol,fmin,'powfunc',/double
  p2[1]=abs(p2[1])
  print,p2,fmin

  shift_sol=interpolate(solar_spec,dindgen(nlambda)+p1[0]/dlam,missing=1.,/double)
  if nparam eq 4 then shift_tell=interpolate(telluric_spec,dindgen(nlambda)+p1[0]/dlam,missing=1.,/double) $
  else shift_tell=interpolate(telluric_spec,dindgen(nlambda)+p1[4]/dlam,missing=1.,/double)
  
  ;  apply h2o factor to telluric spectrum
  
  shift_tell=( 1.0-(1.0-shift_tell)*p1[1] ) > 0.
  
  spec_on=dblarr(nwave)
  spec_off=dblarr(nwave)
  for i=0,nwave-1 do begin
    spec_on[i]=p1[2]*total( filter_trans_on[*,i]*shift_sol*shift_tell )
    spec_off[i]=p1[3]*total( filter_trans_off[*,i]*shift_sol*shift_tell )
  endfor

  plot,wav,obs1,yr=[0.,1.2],psym=2,xtit='CoMP Wavelength (nm)',ytit='Normalized Intensity',tit='Observations and Fit',chars=1.5
  oplot,wav,spec_on
  str=string(format='("Offset:",f9.5," (nm)")',p1[0])
  xyouts,0.1,0.42,str,/norm,chars=1.
  str=string(format='("H2O:",f7.3)',p1[1])
  xyouts,0.1,0.39,str,/norm,chars=1.
  plot,wav,back1,yr=[0.,1.2],psym=2,xtit='CoMP Wavelength (nm)',ytit='Normalized Intensity',tit='Observations and Fit',chars=1.5
  oplot,wav,spec_off
  
  if debug eq 'yes' and dev ne 'p' then read,'enter return',ans

  shift_sol=interpolate(solar_spec,dindgen(nlambda)+p2[0]/dlam,missing=1.,/double)
  if nparam eq 4 then shift_tell=interpolate(telluric_spec,dindgen(nlambda)+p2[0]/dlam,missing=1.,/double) $
  else shift_tell=interpolate(telluric_spec,dindgen(nlambda)+p2[4]/dlam,missing=1.,/double)
  
  ;  apply h2o factor to telluric spectrum
  
  shift_tell=( 1.0-(1.0-shift_tell)*p2[1] ) > 0.
  
  spec_on=dblarr(nwave)
  spec_off=dblarr(nwave)
  for i=0,nwave-1 do begin
    spec_on[i]=p2[2]*total( filter_trans_on[*,i]*shift_sol*shift_tell )
    spec_off[i]=p2[3]*total( filter_trans_off[*,i]*shift_sol*shift_tell )
  endfor

  plot,wav,obs2,yr=[0.,1.2],psym=4,xtit='CoMP Wavelength (nm)',ytit='Normalized Intensity',tit='Observations and Fit',chars=1.5
  oplot,wav,spec_on
  str=string(format='("Offset:",f9.5," (nm)")',p2[0])
  xyouts,0.1,0.13,str,/norm,chars=1.
  str=string(format='("H2O:",f7.3)',p2[1])
  xyouts,0.1,0.1,str,/norm,chars=1.
  xyouts,0.1,0.05,'True Wavelength = CoMP Wavelength + Offset',/norm,chars=1.0
  plot,wav,back2,yr=[0.,1.2],psym=2,xtit='CoMP Wavelength (nm)',ytit='Normalized Intensity',tit='Observations and Fit',chars=1.5
  oplot,wav,spec_off

  if debug eq 'yes' and dev ne 'p' then read,'enter return',ans

  offset[iflat,0]=p1[0]     ;store results in arrays
  h2o[iflat,0]=p1[1]

  offset[iflat,1]=p2[0]
  h2o[iflat,1]=p2[1]

  if nparam eq 5 then begin
    off_tell[iflat,0]=p1[4]
    off_tell[iflat,1]=p2[4]
  endif

endfor

endif else begin
  offset=0.
  h2o=0.
  flat_time=0.
  off_tell=0.
endelse

fits_close,fcb

print,'done'
end

function powfunc,x
common fit, wav, lambda, solar_spec, telluric_spec, filter_trans_on, filter_trans_off, obs, back

debug='no'
ans=' '

nwave=n_elements(wav)
nlambda=n_elements(lambda)
nparam=n_elements(x)

;  shift solar and telluric spectra

dlam=lambda[1]-lambda[0]      ;wavelength spacing of spectra
shift_sol=interpolate(solar_spec,dindgen(nlambda)+x[0]/dlam,missing=1.,/double)
if nparam eq 4 then shift_tell=interpolate(telluric_spec,dindgen(nlambda)+x[0]/dlam,missing=1.,/double) $
  else shift_tell=interpolate(telluric_spec,dindgen(nlambda)+x[4]/dlam,missing=1.,/double)

;  apply h2o factor to telluric spectrum

x[1]=abs(x[1])
shift_tell=( 1.0-(1.0-shift_tell)*x[1] ) > 0.

spec_on=dblarr(nwave)
spec_off=dblarr(nwave)
for i=0,nwave-1 do begin
  spec_on[i]=x[2]*total( filter_trans_on[*,i]*shift_sol*shift_tell )
  spec_off[i]=x[3]*total( filter_trans_off[*,i]*shift_sol*shift_tell )
endfor

if debug eq 'yes' then begin
  plot,lambda,shift_sol,chars=2
  plot,lambda,shift_tell,chars=2
  plot,wav,spec_on,psym=4,chars=2
  oplot,wav,obs
  read,'enter return',ans
endif

chisq=total((spec_on-obs)^2+(spec_off-back)^2)
;chisq=total((spec_on-obs)^2)
return,chisq
end