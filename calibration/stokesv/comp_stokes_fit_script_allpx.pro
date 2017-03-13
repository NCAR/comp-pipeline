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

if(n_elements(date_dir) eq 0) then date_dir = '20141111'
filename = '/hao/solar4/plowman/CoMP/process/'+date_dir+'/level1/coaligned_average.comp.1074.fts'

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

@comp_crosstalk_script.pro ; Compute the crosstalk correction.
imagearr[*,*,15:19]=vcorr ; Update Stokes V data with correction.

comp_make_mask2,sxpar(header0,'DATE-OBS'),header0,mask0,occ_fac=1.00,fld_fac=0.98

nt2 = 100
wmin = min(waves)
wmax = max(waves)
waves2 = wmin+(wmax-wmin)*findgen(nt2)/(nt2-1.0)
x=0
y=0
wcen = waves(floor(0.5*nt))
nimg = n_elements(imagearr[0,0,*])


; Rebin the images and variances, if desired, by changing nx_rebin and ny_rebin to a multiple of 620:
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
nx = n_elements(imagearr_rebin[*,0,0])
ny = n_elements(imagearr_rebin[0,*,0])
mask0_rebin = rebin(mask0,nx,ny)
chisqarr = dblarr(nx,ny)
fitarr = dblarr(nx,ny,nstokes*nt)
parmarr = dblarr(nx,ny,npar)
parmerrarr = dblarr(nx,ny,npar)
statarr = dblarr(nx,ny)

for i=0,nx-1 do begin &$
	for j=0,ny-1 do begin &$
		intens = reform(imagearr_rebin[i,j,0:(nstokes*nt-1)]) &$
		vars = reform(vararr_rebin[i,j,0:(nstokes*nt-1)]) > 0.0 &$
		if(mask0_rebin[i,j] ne 0) then begin &$
			comp_line_fit, waves, intens, vars, a, aerr, chi2, fit &$
			chisqarr[i,j] = total((fit-intens)^2.0/vars)/(n_elements(fit)-n_elements(a)) &$
			fitarr[i,j,*] = fit[0:nstokes*nt-1] &$
			parmarr[i,j,*] = a &$
			parmerrarr[i,j,*] = aerr &$
			statarr = fitarr[nstokes*nt] &$
		endif &$
	endfor &$
	print,i &$
endfor

set_plot,'ps'
device, filename = date_dir+'chisq.eps',/encapsulated, bits=8, /inches, xsize=6, ysize=6
plot_image,chisqarr,min=0,max=10,title = date_dir+'Chi Squared; max = 10'
device,/close
set_plot,'x'
