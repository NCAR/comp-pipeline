; This script computes the coalignment parameters for a set of CoMP L1 files. 
; The files are determined by looking for a GBU file in process_dir/date_dir/level1,
; and the coalignment parameters computed are stored in that same directory as
; (for example) coalign_parms1074.txt. Alternatively, these can be overridden
; by changing filenames (for the files to coalign) and parm_filename (for the
; coalignment parameter output file. Also places jpgs of the coalignment
; into process_dir/date_dir/level1/aligned_jpgs. Override 'jpgout_dir' to put
; these aligned images elsewhere.

.compile util_code/estimate_quantile.pro ; Estimates the quantiles of an image. Used for setting intensity range in plots.
.compile util_code/convert_ascii_header.pro ; Turns an ASCII fits header into a structure.
.compile util_code/comp_estimate_var.pro
.compile util_code/comp_read_data.pro
.compile util_code/get_comp_good_files.pro
.compile util_code/comp_make_mask2.pro ; Modified version of comp_make_mask which allows varying the mask radii.
.compile util_code/guess_background_filename.pro
.compile coalign_code/comp_coregister.pro ; Codes for the actual coregistration.
.compile coalign_code/make_comp_coaligned_average.pro

; required to run Joe's scripts
devicelib
imagelib
device, decomposed=0

wavestr = '1074' ; Which wavelength to use (at present, this is pretty much always '1074')
;if(n_elements(date_dir) eq 0) then date_dir = '20141111' ; Which date directory to check...
if(n_elements(date_dir) eq 0) then date_dir = '20141029' ; Which date directory to check...
process_dir = '/hao/mahidata1/Data/CoMP/process.joe/' ; The top-level directory to look for processed files
directory = process_dir+date_dir+'/level1/' ; The subdirectory where the processed files reside
gbufile = 'GBU.'+wavestr+'.log' ; The name of the GBU file
fits_suffix = '.comp.'+wavestr+'.fts' ; How the files with our wavelength are named
parm_filename = directory+'coalign_parms'+wavestr+'.txt' ; The directory to store the coalignment parameters

tend = 235900 ; Don't use files after this time
; Review the GBU file for which files to use:
filenames=get_comp_good_files(directory+gbufile,dates=dates,times=times, bgs=bgs, ps=ps, nwaves=nwaves)

; Downselect the filenames with additional criteria (time, number of waves, etc):
filecheck = where((dates eq date_dir) and (long(times) lt tend) and (nwaves eq 5))
filenames = filenames[filecheck]
bgs = bgs[filecheck]
ps = ps[filecheck]
dates = dates[filecheck]
times = times[filecheck]
nwaves = nwaves[filecheck]

; Form full filenames:
filenames = directory+filenames
nfiles = n_elements(filenames)

; Guess the background filenames:
bg_filenames = filenames
for i=0,nfiles-1 do bg_filenames[i] = guess_background_filename(filenames[i])

;Use the Stokes IV file with the lowest background as the reference image for coalignment:
iref = where(ps eq 'iv')
vbgmin = min(bgs[iref],imin)
iref = iref[imin]
ref_file_1074=filenames[iref]
print,'Using file number '+strtrim(string(iref),2)+', '+ref_file_1074+' as reference file'

; Find the coalignment parameters for each file. Consists of horizontal shift, vertical shift, and tilt,
; in that order. This will align the current image with the reference image and the previous image, except for
; the first image, which will only use the reference image.
reset=1
delvar,guess
coalign_parms = dblarr(nfiles,4)
openw,lun,parm_filename,/get_lun, width=256
for i=0,nfiles-1 do begin &$
	if(filenames[i] ne ref_file_1074) then begin &$
		coalign_parms[i,*] = comp_coalign_2files(ref_file_1074,filenames[i],/show_result, mask=mask, guess=guess, reset=reset) &$
	endif &$
	printf, lun, filenames[i], string(transpose(coalign_parms[i,*]), format='(4(e26.16))') &$
	flush,lun &$
endfor
free_lun,lun

; Write out jpgs of the aligned images, so we can see how we did:
jpgout_dir = directory+'aligned_jpgs/'
if(~file_test(jpgout_dir)) then file_mkdir,jpgout_dir
write_aligned_jpg_sequence,filenames,iref,coalign_parms,mask,jpgout_dir+'frame'

