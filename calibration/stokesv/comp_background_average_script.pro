.compile util_code/estimate_quantile.pro ; Estimates the quantiles of an image. Used for setting intensity range in plots.
.compile util_code/convert_ascii_header.pro ; Turns an ASCII fits header into a structure.
.compile util_code/comp_estimate_var.pro
.compile util_code/comp_read_data.pro
.compile util_code/get_comp_good_files.pro
.compile util_code/comp_make_mask2.pro ; Modified version of comp_make_mask which allows varying the mask radii.
.compile util_code/guess_background_filename.pro
.compile coalign_code/comp_coregister.pro ; Codes for the actual coregistration.
.compile coalign_code/make_comp_coaligned_average.pro

wavestr = '1074'
if(n_elements(date_dir) eq 0) then date_dir = '20141111'
process_dir = '/hao/solar4/plowman/CoMP/process/'
directory = process_dir+date_dir+'/level1/'
gbufile = 'GBU.'+wavestr+'.log'
fits_suffix = '.comp.'+wavestr+'.fts'
fitsoutname = directory+'coaligned_average'+'.comp.'+wavestr+'.bkg.fts'
varoutname = directory+'coaligned_average'+'.comp.'+wavestr+'.bkg.var.fts'
parm_filename = directory+'coalign_parms'+wavestr+'.txt'


;tend = 193000 ; Don't use files after this time
tend = 235900 ; Don't use files after this time
; Read the lines from the file:
filenames=get_comp_good_files(directory+gbufile,dates=dates,times=times, bgs=bgs, ps=ps, nwaves=nwaves)
filecheck = where((dates eq date_dir) and (long(times) lt tend) and (nwaves eq 5))
;filecheck = where((long(times) lt tend) and (nwaves eq 5))
filenames = filenames[filecheck]
bgs = bgs[filecheck]
ps = ps[filecheck]
dates = dates[filecheck]
times = times[filecheck]
nwaves = nwaves[filecheck]

filenames = directory+filenames
nfiles = n_elements(filenames)

bg_filenames = filenames
for i=0,nfiles-1 do bg_filenames[i] = guess_background_filename(filenames[i])

;Use the Stokes IV file with the lowest background as the reference image for coalignment:
if(n_elements(iref) eq 0) then begin &$
	iref = where(ps eq 'iv') &$
	vbgmin = min(bgs[iref],imin) &$
	iref = iref[imin] &$
endif
ref_file_1074=bg_filenames[iref]
print,'Using file number '+strtrim(string(iref),2)+', '+ref_file_1074+' as reference file'

;coalign_parms = dblarr(nfiles,4)
;chi2s = coalign_parms[*,3]

if(n_elements(chi2_max) eq 0) then chi2_max=20
pols = ['BKGI','BKGQ','BKGU','BKGV']
imageout = make_comp_coaligned_average(bg_filenames,iref,coalign_parms,headerout=headerout, chi2_thold=chi2_max, varout=varout, pols=pols)

fits_open,filenames[iref],fcb
fits_read,fcb,image,header,exten_no=0
fits_close,fcb

nextend = n_elements(headerout[0,*])
fits_open,fitsoutname,fcbout, /write
fits_write,fcbout,image,header
for i=0,nextend-1 do fits_write,fcbout,imageout[*,*,i],headerout[*,i]
fits_close,fcb
print,'Wrote background average to '+fitsoutname

fits_open,varoutname,fcbout, /write
fits_write,fcbout,image,header
for i=0,nextend-1 do fits_write,fcbout,varout[*,*,i],headerout[*,i] &$
fits_close,fcb
print,'Wrote background average variance to '+varoutname

