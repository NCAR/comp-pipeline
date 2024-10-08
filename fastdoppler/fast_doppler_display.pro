pro fast_doppler_display


;  procedure to display fast cadence doppler images and display them


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

date_dir='20140107'

comp_initialize, date_dir
comp_paths, date_dir

ans=' '
debug=0


window,0,xs=nx,ys=ny,title='Velocity'
;window,1,xs=nx,ys=ny,xpos=1935,ypos=650,title='Red-Blue'
window,2,xs=nx,ys=ny,title='Blue Intensity'
window,3,xs=nx,ys=ny,xpos=0,ypos=650,title='Red Intensity'

process_dir = process_basedir + date_dir
name=process_dir+'/'+date_dir+'.080257.ca.fts'

out_dir= process_dir + '/Movie/'

;  open fits file

fits_open,name,fcb       ;open input fits file
num_images=fcb.nextend
if debug eq 1 then print,num_images,' images in file'


;  take inventory of contents of file

inventory,fcb,beam,group,wave,pol,type,expose,cover,cal_pol,cal_ret

uniq_beam=beam[uniq(beam,sort(beam))]	;find unique beams and wavelengths
uniq_wave=wave[uniq(wave,sort(wave))]
nbeam=n_elements(uniq_beam)
nwave=n_elements(uniq_wave)

if debug eq 1 then begin
	print,'beams:',uniq_beam
	print,'wavelengths:',uniq_wave
endif


;  read primary header

fits_read,fcb,d,primary_header,/header_only,exten_no=0


;  compute velocity constant

dlambda=abs(uniq_wave[1]-uniq_wave[0])		;wavelength separation (nm)
l_rest=1074.6	;rest wavelength (nm)
width=0.14		;assumed linewidth (nm)
const=3.e5*width^2/(2.*dlambda*l_rest)		;velocity constant (km/s)


comp_make_mask, date_dir, primary_header, mask
good=where(mask eq 1.,complement=bad)

n=nwave*nbeam
for i=0,num_images-1,n do begin

    fits_read,fcb,blue,header,exten_no=i+1
    fits_read,fcb,red,header,exten_no=i+2

	if nbeam gt 1 then begin		;read other beam if it is there
	    fits_read,fcb,blue2,header,exten_no=i+3
    	fits_read,fcb,red2,header,exten_no=i+4

		blue=(blue+blue2)/2.
		red=(red+red2)/2.
	endif

	vel=const*alog(red/blue)		;compute velocity assuming constant line width


	if i eq 0 then begin		;if first time through, define good pixels
		intens=(red+blue)/2.
		good=where(mask eq 1 and intens gt 1.,complement=bad)
		intens[bad]=0.
		bright=where(intens gt 8.)

		vel_0=vel		;save first velocity image in series for detrending
	endif

	blue[bad]=0.
	red[bad]=0.

;	vel=vel-vel_0		;subtract first velocity image
	vel=vel-median(vel[bright])
	vel[bad]=0.

    wset,2
    tv,bytscl(blue,0.,25.)

	wset,3
	tv,bytscl(red,0.,25.)

;	diff=red-blue		;compute intensity difference
;	diff=diff-median(diff[good])
;	diff[bad]=0.
;	wset,1
;	diff=bytscl(diff,-5,5)
;	tv,diff
  ;
	wset,0
	bvel=bytscl(vel,-10,10)
	tv,bvel

	out_name = out_dir + string(format='(i4.4,".bmp")',i/n)
	write_bmp,out_name,bvel

	wait,0.005

endfor

fits_close,fcb
flush,2

print,'done'

end
