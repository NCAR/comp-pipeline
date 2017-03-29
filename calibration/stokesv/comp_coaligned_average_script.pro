; This script reads a coalignment parameter file produced by comp_coalign_script.pro
; and averages all the files found therein with chi squared less than chi2_max. The
; script assumes a file structure like that of the standard L1 pipeline's
; process directory, and it writes the output aligned files to the process directory.
; The outputs are the on-band average (coaligned_average.comp.1074.fts), onband variance
; (coaligned_average.comp.1074.var.fts), continuum average (coaligned_average.comp.1074.bkg.fts),
; and continuum variance (coaligned_average.comp.1074.bkg.var.fts).

.compile util_code/estimate_quantile.pro ; Estimates the quantiles of an image. Used for setting intensity range in plots.
.compile util_code/convert_ascii_header.pro ; Turns an ASCII fits header into a structure.
.compile util_code/comp_estimate_var.pro
.compile util_code/comp_read_data.pro
.compile util_code/get_comp_good_files.pro
.compile util_code/comp_make_mask2.pro ; Modified version of comp_make_mask which allows varying the mask radii.
.compile util_code/guess_background_filename.pro
.compile coalign_code/comp_coregister.pro ; Codes for the actual coregistration.
.compile coalign_code/make_comp_coaligned_average.pro
.compile coalign_code/comp_read_coalign_parms.pro

; Set up the file names:
if(n_elements(process_dir) eq 0) then process_dir = '/hao/mahidata1/Data/CoMP/process.joe/' ; The top-level directory to look for processed files
if(n_elements(wavestr) eq 0) then wavestr = '1074' ; Which wavelength to use (at present, this is pretty much always '1074')
;if(n_elements(date_dir) eq 0) then date_dir = '20141111' ; Which date directory to check...
if(n_elements(date_dir) eq 0) then date_dir = '20141029' ; Which date directory to check...
directory = process_dir+date_dir+'/level1/' ; The subdirectory where the processed files reside
fits_suffix = '.comp.'+wavestr+'.fts' ; How the files with our wavelength are named
fitsoutname = directory+'coaligned_average'+'.comp.'+wavestr+'.fts' ; Which file to store the coaligned average file
varoutname = directory+'coaligned_average'+'.comp.'+wavestr+'.var.fts' ; Which file to store the variance resulting from averaging
bgfitsoutname = directory+'coaligned_average'+'.comp.'+wavestr+'.bkg.fts' ; As above, but for background (continuum)
bgvaroutname = directory+'coaligned_average'+'.comp.'+wavestr+'.bkg.var.fts' ; As above, but for background (continuum)

parm_filename = directory+'coalign_parms'+wavestr+'.txt' ; The directory from which to read the coalignment parameters

; Read the coalign parameters:
coalign_parms = comp_read_coalign_parms(parm_filename, filenames)
nfiles = n_elements(filenames)
chi2s = coalign_parms[*,3]
chi2min = min(chi2s,iref)
if(n_elements(chi2_max) eq 0) then chi2_max=20

; Foreground (line) file averaging:
; Make the coaligned average (reject files with a reduced Chi squared over chi2_max from the averaging):
imageout = make_comp_coaligned_average(filenames,iref,coalign_parms,headerout=headerout, chi2_thold=chi2_max, varout=varout)

; Write the aligned files
fits_open,filenames[iref],fcb
fits_read,fcb,image,header,exten_no=0
fits_close,fcb

nextend = n_elements(headerout[0,*])
fits_open,fitsoutname,fcbout, /write
fits_write,fcbout,image,header
for i=0,nextend-1 do begin &$
	print,i &$
	fits_write,fcbout,imageout[*,*,i],headerout[*,i] &$
endfor
print,'Wrote line averaged data to '+fitsoutname
fits_close,fcb

fits_open,varoutname,fcbout, /write
fits_write,fcbout,image,header
for i=0,nextend-1 do begin &$
	print,i &$
	fits_write,fcbout,varout[*,*,i],headerout[*,i] &$
endfor
print,'Wrote line averaged variance to '+varoutname
fits_close,fcb

; Background (continuum) file averaging:
bg_filenames = filenames
for i=0,nfiles-1 do bg_filenames[i] = guess_background_filename(filenames[i])

; Make the coaligned background average:
bgpols = ['BKGI','BKGQ','BKGU','BKGV']
bgimageout = make_comp_coaligned_average(bg_filenames,iref,coalign_parms,headerout=bgheaderout, chi2_thold=chi2_max, varout=bgvarout, pols=bgpols)

fits_open,bg_filenames[iref],fcb
fits_read,fcb,bgimage,bgheader,exten_no=0
fits_close,fcb

nextend = n_elements(headerout[0,*])
fits_open,bgfitsoutname,fcbout, /write
fits_write,fcbout,bgimage,bgheader
for i=0,nextend-1 do fits_write,fcbout,bgimageout[*,*,i],bgheaderout[*,i]
fits_close,fcb
print,'Wrote background average to '+bgfitsoutname

fits_open,bgvaroutname,fcbout, /write
fits_write,fcbout,bgimage,bgheader
for i=0,nextend-1 do fits_write,fcbout,bgvarout[*,*,i],bgheaderout[*,i] &$
fits_close,fcb
print,'Wrote background average variance to '+bgvaroutname

