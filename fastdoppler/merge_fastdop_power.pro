;
; Name: merge_fastdop_power
;
; Description: to get power spectrum by merging different time series on the same day
;               (each file contains 9-minutes' observation)
;
; Input Parameters:
;    date_dir - date to process, in YYYYMMDD format
;    time_str - time to process in HHMMSS format
;    ix - the x position of the center of the region chosen to derive average
;         power spectrum (in pixel)
;    iy - the y position of the center
;    dx - the x radius of the region
;    dy - the y radius of the region
;    n_files - the numbers of files to be merged
;
; Others: one can also set if Hann function apodization is needed.
;
; Author: Zihao Yang (modified from the procedure 'fastdop_power_interact' written by Steve Tomczyk)
;





pro merge_fastdop_power, date_dir, time_str, ix, iy, dx, dy, n_files, smooth_s=smooth_s, hann_status=hann_status

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

window,0,xs=nx,ys=ny,retain=2,xpos=0,ypos=550,title='Velocity'
window,1,xs=nx,ys=ny,retain=2,xpos=nx+20,ypos=550,title='Intensity'
window,2,xs=620,ys=500,xpos=0,ypos=0,title='Time Series'
window,3,xs=620,ys=500,xpos=640,ypos=0,title='Power Spectrum'
device,decomposed=0

str=' '

process_dir = process_basedir + date_dir
filenames=process_dir+'/1074_files copy.txt'          ;file with list of filenames`
;n_files = file_lines(filenames)

num_images=0

openr,1,filenames

for ifile=0,n_files-1 do begin
  readf,1,str
  name=process_dir+'/'+strmid(str,0,15)+'.vel.ca.fts'    ;format input filename
  print,name

  fits_open,name,fcb        ;open input fits file
  fits_read,fcb,d,primary_header,/header_only,exten_no=0        ;  read primary header
  num_images=fcb.nextend/2+num_images
endfor
free_lun,1
print,num_images

vel=fltarr(nx,ny,num_images,/nozero)
intens=fltarr(nx,ny,num_images,/nozero)
avg_intens=fltarr(nx,ny)

openr,1,filenames
for ifile=0,n_files-1 do begin
  readf,1,str
  name=process_dir+'/'+strmid(str,0,15)+'.vel.ca.fts'    ;format input filename
  print,name

  fits_open,name,fcb        ;open input fits file
  num=fcb.nextend/2

  for i=0+ifile*num,(num-1)+ifile*num do begin
    fits_read,fcb,dat,header,(i-ifile*num)*2+1
    intens[*,*,i]=dat
    fits_read,fcb,dat,header,(i-ifile*num)*2+2
    vel[*,*,i]=dat
  endfor

  for i=(num/2-5)+ifile*num,num/2+4+ifile*num do begin
    avg_intens=avg_intens+intens[*,*,i]
  endfor
  avg_intens=avg_intens/10.
endfor
free_lun,1
;avg_intens=avg_intens/(ifile+1)

name=process_dir+'/'+date_dir+'.'+time_str+'.vel.ca.fts'
fits_open,name,fcb
fits_read,fcb,d,primary_header,/header_only,exten_no=0

comp_make_mask, date_dir, primary_header, msk      ;make mask from header values
good=where(msk eq 1. and avg_intens gt 5.,complement=bad,count)
print,count
msk[bad]=0.

for i=0,num_images-1 do begin
  v=vel[*,*,i]
  vel[*,*,i]=vel[*,*,i]-mean(v[good])
  v=vel[*,*,i]
  v[bad]=0
  vel[*,*,i]=v
endfor

navg=sxpar(primary_header,'NAVERAGE')     ;number of images averaged
cadence=0.45*float(navg)            ;cadence (sec)
freq=findgen(num_images/2)/(2.*cadence*float(num_images/2))   ;frequency scale
help,freq
time=findgen(num_images)*cadence
nfreq=n_elements(freq)
dnu=freq(1)-freq(0)
print,max(freq)

wset,0
img1=bytscl(msk*vel[*,*,0],-10,10)
tvscl,img1
;oplot,[ix-dx,ix+dx],[iy-dy,iy+dy],linestyle=1,color=255
wset,1
img2=bytscl(sqrt(intens[*,*,0]),0,4)
tvscl,img2
;oplot,[ix-dx,ix+dx],[iy-dy,iy+dy],linestyle=1,color=0

wset,1
print,ix,iy

avg_v_pow=fltarr(num_images/2)
avg_i_pow=fltarr(num_images/2)


hann=hanning(num_images,/double) ;Hann fucntion (if needed)
;
if smooth_s eq 1 then begin
  vel=smooth(vel,[3,3,1])
endif

; data=vel[*,*,*]
; save,filename=date_dir+'powspec.sav',data
; stop

if hann_status eq 1 then begin
  for i=-dx,dx do for j=-dy,dy do begin

    v=vel[ix+i,iy+j,*]
    v=v-mean(v)
    ;v=smooth(v,3)
    pow=abs(fft(hann*v))^2
    pow=pow(0:num_images/2-1)
    avg_v_pow=avg_v_pow+pow

  endfor
endif else begin
  for i=-dx,dx do for j=-dy,dy do begin

    v=vel[ix+i,iy+j,*]
    v=v-mean(v)
    ;v=smooth(v,3)
    pow=abs(fft(v))^2
    pow=pow(0:num_images/2-1)
    avg_v_pow=avg_v_pow+pow

  endfor
endelse

avg_v_pow=avg_v_pow/((2.*dx+1)*(2.*dy+1))

avg_i_pow=avg_i_pow/(2.*dx+1)^2
help,avg_v_pow
help,freq

wset,2
plot,time,v,xtit='Time (s)',ytit='Velocity (km/s)',yr=[-5,5],tit=string(stdev(v))

wset,3
plot_oo,freq,avg_v_pow,xtit='Frequency (Hz)',ytit='Power',xr=[1.e-3,1.],yr=[1.e-5,1.e-1]

print,'done'
end
