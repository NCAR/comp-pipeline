;+
;  Name: fastdop_power
;
;  Description:
;    Procedure to plot power spectra of CoMP fast doppler data for a chosen area.
;
;  Input Parameters:
;    date_dir - date to process, in YYYYMMDD format
;    time_str - time to process in HHMMSS format
;
;  Keyword Parameters: none
;
;  Output: none
;
;
;  Examples:
;    fastdop_power, '20140102', '120956'
;-
pro fastdop_power, date_dir, time_str, ix, iy, dx, dy

common comp_constants, nx, ny, $
  center1074, center1079, center1083,  $
  stokes, n_stokes, debug, int_thresh, $
  diff_thresh

common comp_paths, bias_dir, flat_dir, mask_dir, binary_dir, $
  defered_file, hot_file, $
  ldm_basedir, raw_basedir, process_basedir, hpss_gateway, $
  archive_dir, movie_dir, fullres_dir, log_dir, $
  ffmpeg_dir, logo_dir

common mask_constants, post_rotation, occulter_offset, field_offset, field_overlap, plate_scale


comp_initialize, date_dir			;initialize for data reduction
comp_paths, date_dir

;  create windows

window,0,xs=nx,ys=ny,retain=2,xpos=0,ypos=550,title='Velocity'
window,1,xs=nx,ys=ny,retain=2,xpos=nx+20,ypos=550,title='Intensity'
window,2,xs=1240,ys=500,xpos=0,ypos=0,title='Time Series'
window,3,xs=620,ys=500,xpos=640,ypos=0,title='Power Spectrum'
device,decomposed=0


process_dir = process_basedir + date_dir
print,process_dir       ;processing directory
name=process_dir+'/'+date_dir+'.'+time_str+'.vel.ca.fts'     ;input filename

fits_open,name,fcb          ;open input fits file
nextend=fcb.nextend      ;find number of extensions

fits_read,fcb,d,primary_header,/header_only,exten_no=0        ;  read primary header

num=nextend/2
vel=fltarr(nx,ny,num,/nozero)     ;create arrays for velocity and intensity
intens=fltarr(nx,ny,num,/nozero)

;  read in all images in this file

print,'reading images...'
for i=0,num-1 do begin
  fits_read,fcb,dat,header,i*2+1
  intens[*,*,i]=dat
  fits_read,fcb,dat,header,i*2+2
  vel[*,*,i]=dat
endfor

print,'removing average velocity...'

;  find average of 10 intensity images near center
avg_intens=fltarr(nx,ny)
for i=num/2-5,num/2+4 do begin
  avg_intens=avg_intens+intens[*,*,i]
endfor
avg_intens=avg_intens/10.

;  define good pixels

comp_make_mask, date_dir, primary_header, msk      ;make mask from header values
good=where(msk eq 1. and avg_intens gt 5.,complement=bad,count)
print,count
msk[bad]=0.


;  remove mean of good pixels from every doppler image

for i=0,num-1 do begin
  v=vel[*,*,i]
  vel[*,*,i]=vel[*,*,i]-mean(v[good])
  v=vel[*,*,i]
  v[bad]=0
  vel[*,*,i]=v
endfor

navg=sxpar(primary_header,'NAVERAGE')     ;number of images averaged
cadence=0.45*float(navg)            ;cadence (sec)
freq=findgen(num/2)/(2.*cadence*float(num/2))   ;frequency scale
time=findgen(num)*cadence
nfreq=n_elements(freq)
dnu=freq(1)-freq(0)
;
; vel=smooth(vel,[3,3,1])
;
velocity=vel
save,filename=time_str+'_vel.sav',velocity,time,msk
stop


;  display first intensity and doppler images

wset,0
img1=bytscl(msk*vel[*,*,0],-10,10)
tvscl,img1
oplot,[ix-dx,ix+dx],[iy-dy,iy+dy],linestyle=1,color=255
wset,1
img2=bytscl(sqrt(intens[*,*,0]),0,4)
tvscl,img2
oplot,[ix-dx,ix+dx],[iy-dy,iy+dy],linestyle=1,color=0


  wset,1
  print,ix,iy

  avg_v_pow=fltarr(num/2)
  avg_i_pow=fltarr(num/2)

vel=smooth(vel,[3,3,3])

hann=hanning(num,/double)

  for i=-dx,dx do for j=-dy,dy do begin

    v=vel[ix+i,iy+j,*]
    v=v-mean(v)
    pow=abs(fft(hann*v))^2
    pow=pow(0:num/2-1)
    avg_v_pow=avg_v_pow+pow

;    v=vel[ix+i,iy+j,*]
;    v=v-mean(v)
;    pow=abs(fft(v))^2
;    pow=pow(0:num/2-1)
;    avg_i_pow=avg_i_pow+pow

  endfor
  avg_v_pow=avg_v_pow/(2.*dx+1)^2
  ;print,avg_v_pow
  avg_i_pow=avg_i_pow/(2.*dx+1)^2

  wset,2
  plot,time[0:100],v[0:100],xtit='Time (s)',ytit='Velocity (km/s)',yr=[-1.5,1.5],tit=string(stdev(v))

  wset,3
  plot_oo,freq,avg_v_pow,xtit='Frequency (Hz)',ytit='Power',xr=[1.e-3,1.],yr=[1.e-4,1.e-1]


;  average power spectra over all good pixels

; avg_pow=fltarr(num/2)
; for i=0,count-1 do begin
;   ix=good[i] mod nx
;   iy=fix(good[i]/nx)
;   v=vel[ix,iy,*]
;   v=v-mean(v)
;   pow=abs(fft(v))^2
;   pow=pow(0:num/2-1)
;
;   avg_pow=avg_pow+pow
; endfor
; avg_pow=avg_pow/float(count)
;
; wset,3
; plot_oo,freq,avg_pow,xtit='Frequency (Hz)',ytit='Power',xr=[1.e-3,1.],yr=[1.e-4,1.]
;

print,'done'

end
