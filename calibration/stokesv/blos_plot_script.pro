.compile util_code/comp_make_mask2

I = parmarr[*,*,4]
L = sqrt(parmarr[*,*,5]^2.0+parmarr[*,*,6]^2.0)
V = parmarr[*,*,8]

Verr = sqrt(2.0*total(vararr_rebin[*,*,15:19],3))/0.5/(total(imagearr_rebin[*,*,0:4],3))
Verr_rel = Verr/total(abs(imagearr_rebin[*,*,15:19]),3)

clrmapvals = 255*(dindgen(127)/126.0)
clrmapvals2 = 254*(dindgen(63)/62.0)

red=[0,reverse(clrmapvals),dblarr(127),255]
;green=[0,0.05*reverse(clrmapvals),0.05*clrmapvals,255]
green=[0,dblarr(127),clrmapvals,255]
blue=red
tvlct,red,green,blue

g=1.5
e0=4.8e-10
me=9.11e-28
c=3.0e10
nmfac = 1.0e-7
lambdas = parmarr[*,*,0]*nmfac
lwids = parmarr[*,*,1]*nmfac

Vscal = V*(4.0*!pi*me*c*c/(lambdas*lambdas*e0))

comp_make_mask2,sxpar(header0,'DATE-OBS'),header0,mask0,occ_fac=1.00,fld_fac=0.95
mask0_rebin = rebin(mask0,nx,ny)

blos = 2.0*Vscal/(L+I)/3.0

!p.multi=0

set_plot,'z'
device,set_resolution=[1280,896]

if(n_elements(stokesimin) eq 0) then stokesimin = 10
if(n_elements(vrelerr_max) eq 0) then vrelerr_max = 0.1
mask = mask0_rebin*(Verr_rel lt vrelerr_max)*(I gt stokesimin)
plot_image,blos*mask, min=-50,max=50,top=254,bottom=1,scale=[index0.cdelt1*xscl,index0.cdelt2*yscl],origin=origin,ytitle='Solar Latitude (Arcseconds)'

colorbar,bottom=1,ncolors=254,/vertical,range=[-50,50]

pcimg = tvrd()
clrimg = [[[red[pcimg]]],[[green[pcimg]]],[[blue[pcimg]]]]

write_jpeg,'comp_blos_'+date_dir+'.jpg',clrimg,quality=100,true=3

set_plot,'x'

