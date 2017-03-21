; Currently this script is called from inside of comp_stokes_fit_script_allpx.pro and comp_stokes_fit_script_pxpeep.pro.
; It may work to call it on its own if filename is specified, but no guarantees - there are probably bugs or omissions.
.compile ../../src/level1/comp_get_component.pro
.compile comp_find_vxtalk.pro ; This is probably equivalent to the routine of the same name in my new pipeline branch.
.compile ../../src/level1/comp_inventory_header.pro

; Read the data:
fgfile = filename
bgfile = guess_background_filename(filename)
comp_read_data, bgfile, bgimagearr, bgheaderarr, bgheader0
comp_read_data, fgfile, fgimagearr, fgheaderarr, fgheader0

header0 = fgheader0
imagearr = fgimagearr
headerarr = fgheaderarr

nx = n_elements(bgimagearr[*,0,0])
ny = n_elements(bgimagearr[0,*,0])

; Estimate variances if needed:
if(n_elements(vararr) eq 0) then vararr = comp_estimate_var2(fgimagearr, fgheaderarr, bgimagearr, bgheaderarr)
wave = [1074.38,      1074.50,      1074.62,      1074.74,      1074.86] ; Use these lines...

; Unpack background averages:
ibg_avg = comp_get_component(bgimagearr,bgheaderarr,'BKGI',0,wave,/average_wavelengths,/noskip)
qbg_avg = comp_get_component(bgimagearr,bgheaderarr,'BKGQ',0,wave,/average_wavelengths,/noskip)
ubg_avg = comp_get_component(bgimagearr,bgheaderarr,'BKGU',0,wave,/average_wavelengths,/noskip)
vbg_avg = comp_get_component(bgimagearr,bgheaderarr,'BKGV',0,wave,/average_wavelengths,/noskip)
ifg_avg = comp_get_component(fgimagearr,fgheaderarr,'I',0,wave,/average_wavelengths,/noskip)
qfg_avg = comp_get_component(fgimagearr,fgheaderarr,'Q',0,wave,/average_wavelengths,/noskip)
ufg_avg = comp_get_component(fgimagearr,fgheaderarr,'U',0,wave,/average_wavelengths,/noskip)
vfg_avg = comp_get_component(fgimagearr,fgheaderarr,'V',0,wave,/average_wavelengths,/noskip)

; Pull out the variance in Stokes V:
vvar_avg = comp_get_component(vararr, fgheaderarr, 'V', 0, wave, /noskip, /average_wavelengths, navg=navg)
vvar_avg = vvar_avg/navg
vcerrs = sqrt(vvar_avg)

; Make a mask for computing crosstalk:
comp_make_mask2,sxpar(header0,'DATE-OBS'),bgheader0,bgmask0,occ_fac=1.03,fld_fac=0.935

; Mask out pixels with over-large SNR, as defined by the median absolute deviation:
if(n_elements(bgmask_thold) eq 0) then bgmask_thold = 6
snrp1 = 1+abs(vbg_avg)/vcerrs
snrp1_med = median(snrp1[where(bgmask0)])
snrp1_mad = median(abs(snrp1_med-snrp1[where(bgmask0)]))
bgmask = bgmask0*(snrp1 lt snrp1_med+bgmask_thold*snrp1_mad)

; Compute the V crosstalk corrections:
comp_find_vxtalk, ibg_avg, qbg_avg, ubg_avg, vbg_avg, vcerrs, bgmask, $
                      IVxtalk, QVxtalk, UVxtalk, xtparms

; Apply crosstalk corrections to Stokes V:
vxtalk = dblarr(nx,ny,5)
vcorr = imagearr[*,*,15:19]

nxtalks = 3
for i=0,4 do vxtalk[*,*,i] += ivxtalk*(imagearr[*,*,i]);+imagearr[*,*,i+20])
for i=0,4 do vxtalk[*,*,i] += qvxtalk*imagearr[*,*,i+5]
for i=0,4 do vxtalk[*,*,i] += uvxtalk*imagearr[*,*,i+10]

vcorr -= vxtalk

; Plot the corrections:
pmin = -0.05
pmax = 0.05

r=[0,2*reverse(dindgen(127))+1,dblarr(127),255]
g=[0,dblarr(254),255]
b=[0,dblarr(127),2*dindgen(127)+1,255]
tvlct,b,r,b

; Correction applied to center wavelength:
!p.multi=[0,3,0]
set_plot,'z'
device, set_resolution=[2160,720]
plot_image,imagearr[*,*,17]*bgmask0, top=254, bottom=1, min=pmin, max=pmax, title='Stokes V (line center)'
plot_image,vxtalk[*,*,2]*bgmask0, top=254, bottom=1, min=pmin, max=pmax, title='Crosstalk fit (min='+strtrim(string(pmin),2)+',max='+strtrim(string(pmax),2)+')'
plot_image,vcorr[*,*,2]*bgmask0, top=254, bottom=1, min=pmin, max=pmax, title='Residual Stokes V (line center)'

pcimg = tvrd()
clrimg = [[[b[pcimg]]],[[r[pcimg]]],[[b[pcimg]]]]
write_jpeg,'comp_vcrosstalk_correction_fglinecenter'+date_dir+'.jpg',clrimg,quality=100,true=3

; Correction applied to averaged background:
vxtalk_bg = (ibg_avg*ivxtalk+qbg_avg*qvxtalk+ubg_avg*uvxtalk)
device, set_resolution=[2160,720]
plot_image,vbg_avg*bgmask0, top=254, bottom=1, min=pmin, max=pmax, title='Stokes V (background)'
plot_image,vxtalk_bg*bgmask0, top=254, bottom=1, min=pmin, max=pmax, title='Crosstalk fit (min='+strtrim(string(pmin),2)+',max='+strtrim(string(pmax),2)+')'
plot_image,(vbg_avg-vxtalk_bg)*bgmask0, top=254, bottom=1, min=pmin, max=pmax, title='Residual Stokes V (background)'

pcimg = tvrd()
clrimg = [[[b[pcimg]]],[[r[pcimg]]],[[b[pcimg]]]]
write_jpeg,'comp_vcrosstalk_correction_bg'+date_dir+'.jpg',clrimg,quality=100,true=3


set_plot,'x'
!p.multi=0
