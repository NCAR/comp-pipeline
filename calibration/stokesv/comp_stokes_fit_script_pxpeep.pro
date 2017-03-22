.compile util_code/estimate_quantile.pro ; Estimates the quantiles of an image. Used for setting intensity range in plots.
.compile util_code/convert_ascii_header.pro ; Turns an ASCII fits header into a structure.
.compile util_code/comp_make_mask2.pro ; Modified version of comp_make_mask which allows varying the mask radii.
.compile util_code/guess_background_filename.pro
.compile util_code/comp_estimate_var.pro
.compile util_code/comp_read_data.pro
.compile coalign_code/comp_coregister.pro ; Codes for the actual coregistration.
.compile fitting_code/comp_inten_calc.pro
.compile fitting_code/comp_stokes_profiles.pro
.compile fitting_code/comp_line_fit.pro

common comp_inten_calc_comblk, interptab, interptab_deriv, na, nl0, nl, logamin, logamax, l0min, l0max, lmin, lmax

restore, filename='allvars.sav'

; required to run Joe's scripts
devicelib
imagelib

if(n_elements(date_dir) eq 0) then date_dir = '20141111'
filename = '/hao/mahidata1/Data/CoMP/process.joe/'+strtrim(string(date_dir),2)+'/level1/coaligned_average.comp.1074.fts'

; Get the header information, primarily for the extension number:
comp_read_data, filename, imagearr, headerarr, header0
index0 = convert_ascii_header(header0) &$
ntune = max([sxpar(header0,'NTUNE'),sxpar(header0,'NTUNES')]) < 5

nxt = n_elements(headerarr[0,*])
navg = lonarr(nxt)
for i=0,nxt-1 do navg[i] = sxpar(headerarr[*,i],'NAVERAGE')
nt = ntune
nstokes = 4
if(nstokes eq 3) then npar = 7
if(nstokes eq 4) then npar = 10

waves = dblarr(nt)
for i=0,nt-1 do waves[i] = sxpar(headerarr[*,i],'WAVELENG')

fgfile = filename
bgfile = guess_background_filename(filename, fgvar_filename=fgvar_filename, bgvar_filename=bgvar_filename)
comp_read_data, bgfile, bgimagearr, bgheaderarr, bgheader0
comp_read_data, fgfile, fgimagearr, fgheaderarr, fgheader0
comp_read_data, fgvar_filename, vararr, varheaderarr, varheader0
vararr_est = comp_estimate_var2(fgimagearr, fgheaderarr, bgimagearr, bgheaderarr)
imagearr0=imagearr

mg_log, 'before COMP_CROSSTALK_SCRIPT'
@comp_crosstalk_script.pro ; Compute the crosstalk correction.
mg_log, 'after COMP_CROSSTALK_SCRIPT'

imagearr[*,*,15:19]=vcorr ; Update Stokes V data with correction.

comp_make_mask2,sxpar(header0,'DATE-OBS'),header0,mask0,occ_fac=1.00,fld_fac=0.98

waves = dblarr(nt)
for i=0,nt-1 do waves[i] = sxpar(headerarr[*,i],'WAVELENG')

set_plot,'x'
window,0

nx0 = n_elements(imagearr[*,0,0])
ny0 = n_elements(imagearr[0,*,0])
nx_rebin = 620
ny_rebin = 620
xscl = nx0/nx_rebin
yscl = ny0/ny_rebin
if(nx_rebin ne nx0 or ny_rebin ne ny0) then begin &$
	imagearr_rebin = rebin(imagearr,nx_rebin,ny_rebin,nimg) &$
	vararr_rebin = rebin(vararr,nx_rebin,ny_rebin,nimg)/(xscl*yscl) &$
endif else begin &$
	imagearr_rebin = imagearr &$
	vararr_rebin = vararr &$
endelse
ivar_scale = 2.0
quvar_scale = 2.0
redvar_scale = 10.0
bluvar_scale = 2.0
vararr_rebin[*,*,2*nt:3*nt-1] = quvar_scale*vararr_rebin[*,*,2*nt:3*nt-1] 
vararr_rebin[*,*,nt:2*nt-1] = quvar_scale*vararr_rebin[*,*,nt:2*nt-1] 
vararr_rebin[*,*,0:nt-1] = ivar_scale*vararr_rebin[*,*,0:nt-1]
vararr_rebin[*,*,[4,9,14,19]] = redvar_scale*vararr_rebin[*,*,[4,9,14,19]] ; Increase the uncertainty in the red wing of the spectral line, since it tends to have issues...
vararr_rebin[*,*,[0,5,10,15]] = bluvar_scale*vararr_rebin[*,*,[0,5,10,15]] ; Increase the uncertainty in the blue wing of the spectral line, since it tends to have issues...

red=[0,reverse(clrmapvals),dblarr(127),255]
green=[0,dblarr(127),clrmapvals,255]
blue=red


nx = n_elements(imagearr_rebin[*,0,0])
ny = n_elements(imagearr_rebin[0,*,0])
mask0_rebin = rebin(mask0,nx,ny)
nt2 = 100
wmin = min(waves)
wmax = max(waves)
waves2 = wmin+(wmax-wmin)*findgen(nt2)/(nt2-1.0)
x=0
y=0
wcen = waves[floor(0.5*nt)]
wfilt = 2.3/16.0
loadct,0
while !pi gt 0 do begin &$
	wset,0 &$
	!p.multi=0 &$
	if(n_elements(blos) gt 0) then begin &$
                device, decomposed=0 &$
		tvlct,red,green,blue &$
		plot_image,blos*mask, min=-50,max=50,top=254,bottom=1 &$
		colorbar,bottom=1,ncolors=254,/vertical,range=[-50,50] &$
	endif else begin &$
		loadct,0 &$
		if(n_elements(parmarr) eq 0) then plot_image,sqrt(imagearr_rebin[*,*,ceil(0.5*nt)]),min=0,max=3 &$
		if(n_elements(parmarr) gt 0) then plot_image,sqrt(parmarr[*,*,4]),min=0,max=6 &$
	endelse &$
	oplot,[x],[y],psym=1,thick=4,color=0 &$
	oplot,[x],[y],psym=1,color=255 &$
	cursor,x,y,/wait,/data &$
	print,round(x),round(y) &$
	intens = reform(imagearr_rebin[round(x),round(y),0:(nstokes*nt-1)]) &$
	vars = reform(vararr_rebin[round(x),round(y),0:(nstokes*nt-1)]) > 0.0 &$
	print,intens &$
	print,vars &$
	comp_line_fit, waves, intens, vars, a, aerr, chi2, fit &$
;	a = parmarr[round(x),round(y),*] &$
	print,chi2 &$
;	print,a,aerr &$
	print,'Residuals: ' &$
	print,(fit-intens)^2/vars &$

	comp_get_window, 1 &$
	!p.multi = [0,2,2] &$

	for i=0,nstokes-1 do begin &$
		inten = intens[i*nt:i*nt+nt-1] &$
		err = sqrt(vars[i*nt:i*nt+nt-1]) &$
		apeak = a[i+4]*wfilt/sqrt(2.0*!pi*a[1]^2.0) &$
		ploterr, waves, inten, err, psym=1, yrange=1.1*[min([inten,apeak]), max([inten,apeak])] &$
		oplot,waves,fit[i*nt:i*nt+nt-1],psym=4 &$
		fitcurve = (a[i+4]*exp(-0.5*(waves2 - a[0])^2.0/a[1]^2.0)/sqrt(2.0*!pi*a[1]^2.0)) &$
		if(i eq 0) then begin &$
			fitcurve += a[2]+a[3]*(waves2-wcen) &$
		endif &$
		if(i eq 3) then begin &$
			fitcurve += fitcurve*a[8]*(waves2-a[0])/a[1]^2.0/1.0e-7/a[i+4] + a[9]*(a[2]+a[3]*(waves2-wcen)) &$
		endif &$
		oplot,waves2,fitcurve*wfilt,linestyle=2 &$
	endfor &$
	wait,0.1 &$

endwhile

