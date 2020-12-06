;+
;  :Name: cal_opal
;
;  :Description: This routine is used to calibrate the brightness of comp diffuser imaages
;  by comparing opal images to images of the solar disk taken through a calibrated neutral
;  density filter.
;  
;  :Author: tomczyk
;-
pro cal_opal

@comp_constants_common
@comp_config_common
@comp_mask_constants_common

; configure

date_dir='20161214'

comp_initialize, date_dir
;comp_configuration

debug='no'       ;debug mode ('yes' or 'no')
ans=' '

opal_file='130906'         ;opal data       
sun_file='123928'          ;sun data with no occulter and with ND filter

opal_file='130950'         ;opal data
sun_file='124012'          ;sun data with no occulter and with ND filter

opal_file='131031'         ;opal data
sun_file='124052'          ;sun data with no occulter and with ND filter

opal_file='131115'         ;opal data
sun_file='124137'          ;sun data with no occulter and with ND filter


time1=13. + 9./60. + 6./3600.   ;compute times in hours
time2=12. + 39./60. + 28./3600.
time=(time1+time2)/2.   ;find average time


;  open files

raw_dir='V:\Data\CoMP\raw\'+date_dir+'\'
process_basedir='V:\Data\CoMP\process\'
cd,process_basedir

file=raw_dir+date_dir+'.'+opal_file+'.FTS'
print,file
fits_open,file,fcb_opal
num=fcb_opal.nextend

file=raw_dir+date_dir+'.'+sun_file+'.FTS'
print,file
fits_open,file,fcb_sun


;  get dark image

exposure=250.
dark=comp_dark_interp(date_dir,time,exposure)


;  process images

window,0,xs=nx,ys=nx
window,1,xs=nx,ys=nx

x=rebin(findgen(nx),nx,nx)
y=transpose(x)
x0=291    ;  approximate center of sub-image 1
y0=324
r=sqrt((x-x0)^2+(y-y0)^2)

opal_rad=fltarr(num)     ;array to hold diffuser radiance values   

for i=0,num-1 do begin
	fits_read,fcb_opal,dat,header,exten_no=i+1
	if sxpar(header,'DEMULT') eq 0 then dat=demultiplex(dat)
	dat=float(dat)-dark
  dat=fixrock(dat,0.030)
  dat=fix_image(dat)
  opal=dat
  opal1=comp_extract1(opal)
  opal2=comp_extract2(opal)
  
  wset,0
  tvscl,opal1
  
  fits_read,fcb_sun,dat,header,exten_no=i+1
  if sxpar(header,'DEMULT') eq 0 then dat=demultiplex(dat)
  dat=float(dat)-dark
  dat=fixrock(dat,0.030)
  dat=fix_image(dat)
  sun=dat
  sun1=comp_extract1(sun)
  sun2=comp_extract2(sun)

  wset,1
  tvscl,sun1
  
  good=where(r lt 240. and sun1 gt max(sun1)/4.)
  
  disk=fltarr(nx,nx)    ;compute actual sun center
  disk[good]=1.
  x_cent=total(x*disk)/total(disk)
  y_cent=total(y*disk)/total(disk)
  if debug eq 'yes' then print,x_cent,y_cent
  
  r=sqrt( (x-x_cent)^2 + (y-y_cent)^2 )
  center=where(r lt 40.)   ;define sun center area
  disk=median(sun1[center])   ;find sun center intensity
  
  t135=-3.*!pi/4.
  t45=!pi/4.
  theta=atan(y-y_cent,x-x_cent)
  ann=where( (r gt 255. and r lt 275. and theta gt t45-0.2 and theta lt t45+0.2) or $
            (r gt 255. and r lt 275. and theta gt t135-0.2 and theta lt t135+0.2) )    ;define annulus
  diffuser=median(opal1[ann])
  
  if debug eq 'yes' then print,center,diffuser

  opal_rad[i]=diffuser*3.e-5/disk
  print,opal_rad[i]
  
  opal1[good]=sun1[good]
  opal1[ann]=0.
  opal1[center]=0.
  wset,0
  tvscl,opal1
  
endfor

fits_close,fcb_opal
fits_close,fcb_sun

print,'mean:',mean(opal_rad)

print,'done'

end