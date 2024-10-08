pro filter_data,time_str

;  Procedure Fourier filters comp waves images in time and selects a subset of the time series.

ans=' '
debug=0     ;debug mode, 1 for on, 0 for off

dir='/Data/wave/process/20140113/'
; dir='/Volumes/E/wave/process/20140107/'
cd,dir
restore,dir+time_str+'_vel.sav'

s=size(velocity)
nx=s(1)
ny=s(2)
ntime=s(3)

;  subset of data to use

;ntime=300
t1=0 & t2=t1+ntime-1

ntap=30     ;number of points to taper each end of time series

;  filter data with gaussian peaked at freq0 with width in frequency of fwidth

freq=findgen(ntime)/(float(ntime)*29.0)
for i=0,ntime/2-1 do freq(ntime-i-1)=freq(i+1)

freq0=.03 ;Hz
fwidth=.003 ;Hz
filter=exp( -(freq-freq0)^2/fwidth^2 )
filter(0)=0.

if debug eq 1 then begin
  plot,filter
  read,'enter return',ans
endif

; for i=0,nx-1 do for j=0,ny-1 do begin
;   y=reform(intensity(i,j,0:ntime-1))
;   y=y-median(y)
;   taper,y,ntap   ;taper points at start and end of timeseries
;   trans=filter*fft( y )
;   intensity(i,j,0:ntime-1)=fft(trans,/inverse)
; endfor
; intensity=intensity(*,*,0:ntime-1)
; time=time(t1:t2)
; save,intensity,time,file=dir+'intensity_filtered.sav'
; intensity=0

restore,dir+time_str+'_vel.sav'
;bad=where(msk eq 0, complement=good)
; window,0,xs=600,ys=400,xpos=50,ypos=80
; window,1,xs=600,ys=400,xpos=660,ypos=80

for i=0,nx-1 do for j=0,ny-1 do begin
  y=reform(velocity(i,j,0:ntime-1))
  if y[0] ne 0. then begin
    y=y-median(y)
    taper,y,ntap   ;taper points at start and end of timeseries
    ; wset,0
    ; plot,y,yr=[-5,5]
    trans=filter*fft( y )
    velocity(i,j,0:ntime-1)=fft(trans,/inverse)
    ; wset,1
    ; plot,reform(velocity(i,j,0:ntime-1)),yr=[-2,2]
    ; str=''
    ; read,'enter return',str
  endif
endfor
velocity=velocity(*,*,0:ntime-1)
time=time[t1:t2]
save,velocity,time,msk,file=dir+time_str+'_vel_filtered.sav'
velocity=0

; restore,dir+'azimuth_interp.sav'
; for i=0,nx-1 do for j=0,ny-1 do begin
;   y=reform(azimuth(i,j,0:ntime-1))
;   y=y-median(y)
;   taper,y,ntap   ;taper points at start and end of timeseries
;   trans=filter*fft( y )
;   azimuth(i,j,0:ntime-1)=fft(trans,/inverse)
; endfor
; azimuth=azimuth(*,*,0:ntime-1)
; time=time(t1:t2)
; save,azimuth,time,file=dir+'azimuth_filtered.sav'
; azimuth=0
;
; restore,dir+'linewidth_interp.sav'
; for i=0,nx-1 do for j=0,ny-1 do begin
;   y=reform(linewidth(i,j,0:ntime-1))
;   y=y-median(y)
;   taper,y,ntap   ;taper points at start and end of timeseries
;   trans=filter*fft( y )
;   linewidth(i,j,0:ntime-1)=fft(trans,/inverse)
; endfor
; linewidth=linewidth(*,*,0:ntime-1)
; time=time(t1:t2)
; save,linewidth,time,file=dir+'linewidth_filtered.sav'
; linewidth=0
;
; restore,dir+'polarization_interp.sav'
; for i=0,nx-1 do for j=0,ny-1 do begin
;   y=reform(polarization(i,j,0:ntime-1))
;   y=y-median(y)
;   taper,y,ntap   ;taper points at start and end of timeseries
;   trans=filter*fft( y )
;   polarization(i,j,0:ntime-1)=fft(trans,/inverse)
; endfor
; polarization=polarization(*,*,0:ntime-1)
; time=time(t1:t2)
; save,polarization,time,file=dir+'polarization_filtered.sav'
; polarization=0

print,'done'
end
