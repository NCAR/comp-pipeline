ifit = parmarr[*,*,4]
qfit = parmarr[*,*,5]
ufit = parmarr[*,*,6]
vxfit = parmarr[*,*,7]
vxfit_err = parmerrarr[*,*,7]

bgmask = mask0
bgmask0 = bgmask

snrp1 = 1+abs(vxfit)/vxfit_err
snrp1_med = median(snrp1[where(bgmask)])
snrp1_mad = median(abs(snrp1_med-snrp1[where(bgmask)]))
bgmask = bgmask*(snrp1 lt snrp1_med+3*snrp1_mad)

comp_find_vxtalk_standalone, ifit, qfit, ufit, vxfit, vxfit_err, bgmask, $
                      IVxtalk_fit, QVxtalk_fit, UVxtalk_fit, xtparms

r=[0,2*reverse(dindgen(127))+1,dblarr(127),255]
g=[0,dblarr(254),255]
b=[0,dblarr(127),2*dindgen(127)+1,255]
tvlct,b,r,b

pmin = -0.05
pmax = 0.05

vxtalk_fit2 = (ifit*ivxtalk_fit+qfit*qvxtalk_fit+ufit*uvxtalk_fit)

!p.multi=[0,3,0]
set_plot,'z'
device, set_resolution=[2160,720]
plot_image,vxfit*bgmask0, top=254, bottom=1, min=pmin, max=pmax, title='Stokes V (crosstalk from line fit)'
plot_image,vxtalk_fit2*bgmask0, top=254, bottom=1, min=pmin, max=pmax, title='Crosstalk fit (min='+strtrim(string(pmin),2)+',max='+strtrim(string(pmax),2)+')'
plot_image,(vxfit-vxtalk_fit2)*bgmask0, top=254, bottom=1, min=pmin, max=pmax, title='Residual crosstalk in Stokes V line fit'

pcimg = tvrd()
clrimg = [[[b[pcimg]]],[[r[pcimg]]],[[b[pcimg]]]]
write_jpeg,'comp_vcrosstalk_correction_linefit.jpg',clrimg,quality=100,true=3

set_plot,'x'
!p.multi=0
