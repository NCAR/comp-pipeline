;+
;  Name: plot_offsets
;  
;  Description:
;    Procedure to plot coalignment offsets.
;    
;  Input Parameters:
;    date_dir - date to process, in YYYYMMDD format
;    
;  Keyword Parameters: none
;
;  Output: none
;
;  Author: Tomczyk
;
;  Examples:
;    plot_Offsets, '20140102'
;-
pro plot_offsets, date_dir

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

window,0,xs=800,ys=400,retain=2,xpos=0,ypos=0,title='x-offsets'
window,1,xs=800,ys=400,retain=2,xpos=0,ypos=420,title='y-offsets'
window,2,xs=800,ys=400,retain=2,xpos=0,ypos=840,title='theta-offsets'
device,decomposed=0


process_dir = process_basedir + date_dir        ;processing directory

names=file_search(process_dir+'\*.ca.0.sav')
nfiles=n_elements(names)

x=fltarr(10000)
y=fltarr(10000)
theta=fltarr(10000)
num=intarr(nfiles)
i1=0

for i=0,nfiles-1 do begin
  
  print,names[i]
  
  restore,names[i]
  num[i]=n_elements(x_offset)
  
  x[i1:i1+num[i]-1]=x_offset
  y[i1:i1+num[i]-1]=y_offset
  theta[i1:i1+num[i]-1]=theta_offset

  i1=i1+num[i]
  
endfor

npt=total(num)
x=x[0:npt-1]
y=y[0:npt-1]
theta=theta[0:npt-1]

wset,0
plot,x

wset,1
plot,y

wset,2
plot,theta

h=histogram(y,binsize=0.1,locations=xhist,min=-5,max=5)
plot,xhist,h,psym=10

print,'done'

end