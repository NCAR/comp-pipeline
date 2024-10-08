;+
;  Name: fast_doppler_demod
;
;  Description:
;    Procedure to demodulate Level_0 CoMP fast cadence doppler data. This routine 
;    will analyze all files in this date_dir directory and compute
;    calibrated intensity images and output them in fits file format.
;
;  Input Parameters:
;    date_dir - date to process, in YYYYMMDD format
;
;  Keyword Parameters: none
;
;  Output:
;    The calibrated intensity images are written to an output Level_1 FITS file with one 
;    extension for each intensity image.
;
;  Author: Tomczyk
;
;  Examples:
;    fast_doppler_demod, '20140102'
;-
pro fast_doppler_demod, date_dir

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


comp_initialize, date_dir			;initialize for data reduction
comp_paths, date_dir

window,2,xs=nx,ys=ny,title='Intensity'			;open a window to display results


;  find files in directory to analyze

raw_dir  = raw_basedir + date_dir
process_dir = process_basedir + date_dir

filenames=process_dir+'\1074_files.txt'          ;file with list of filenames
n_files = file_lines(filenames)           ;get number of filenames in file

openr,1,filenames       ;open file with filenames
str=' '

names=strarr(n_files)
nfile=0				;identify large files which are the ones to process
for i=0,n_files-1 do begin
  readf,1,str
  names[i]=raw_dir+'\'+strmid(str,0,15)+'.fts'    ;format input filename
	info=file_info(names[i])
	size=info.size
	if size gt 2.5e9 then begin
		names[nfile]=strmid(file_basename(names[i]),9,6)	;extract file time
		nfile=nfile+1
	endif
endfor
names=names[0:nfile-1]
close,1

;  process all files in this directory

nskip=0			;number of files to skip

for ifile=nskip,nfile-1 do begin

	in_file=raw_dir+'\'+date_dir+'.'+names[ifile]+'.fts'
	out_file=process_dir+'\'+date_dir+'.'+names[ifile]+'.l1.fts'


;  open fits file

	fits_open,in_file,fcb       ;open input fits file
	num_images=fcb.nextend
	if debug eq 1 then print,num_images,' images in file'


;  compute transformation arrays for distortion removal

	x=rebin(findgen(nx),nx,nx)
	y=transpose(x)

	k1=0.99353
	x1new=x*.5*(1.+k1)+y*.5*(1.-k1)
	y1new=x*.5*(1.-k1)+y*.5*(1.+k1)

	k2=1.00973
	x2new=x*.5*(1.+k2)+y*.5*(1.-k2)
	y2new=x*.5*(1.-k2)+y*.5*(1.+k2)


;  take inventory of images in this file

	inventory,fcb,beam,group,wave,pol,type,expose,cover,cal_pol,cal_ret


;  read primary header and get time and exposure time

	if debug eq 1 then print,'read header and get time and exposure time'

	fits_read,fcb,d,primary_header,/header_only,exten_no=0
	date_str=sxpar(primary_header,'DATE_OBS')
	time_str=sxpar(primary_header,'TIME_OBS')
	fits_read,fcb,d,header,/header_only,exten_no=1
	exposure=sxpar(header,'EXPOSURE')

	if debug eq 1 then begin
		print,date_str,'   ',time_str
		print,'exposure time:',exposure
	endif


;  interpret date and time

	if debug eq 1 then print,'interpret date and time'

	month=fix(strmid(date_dir,4,2))
	day=fix(strmid(date_dir,6,2))
	year=fix(strmid(date_dir,0,4))
	if year lt 2000 then year=year+2000

	if strlen(time_str) eq 10 then begin
	  hours=fix(strmid(time_str,0,1))
	  mins=fix(strmid(time_str,2,2))
	  secs=fix(strmid(time_str,5,2))
	endif else begin
	  hours=fix(strmid(time_str,0,2))
	  mins=fix(strmid(time_str,3,2))
	  secs=fix(strmid(time_str,6,2))
	endelse

	if hours lt 7 then hours=hours+12
	time=float(hours)+float(mins)/60.+float(secs)/3600.

	if debug eq 1 then begin
		print,hours,mins,secs
		print,time
	endif


;  compute solar ephemeris quantities from date and time

	jd=tojd(day,month,year,hours,mins,secs)
	if debug eq 1 then print,'jd=',jd

	ephem2,jd,sol_ra,sol_dec,b0,p_angle,semi_diam,sid_time,dist,xsun,ysun,zsun


	;  set up matrix for image rotation

	x0=float(nx)/2.
	y0=float(nx)/2.

	x=rebin( findgen(nx)-x0,nx,nx )
	y=transpose(rebin( findgen(nx)-y0,nx,nx ))

	angle=p_angle+180.
	xp=x*cos(angle*!pi/180.) -y*sin(angle*!pi/180.)
	yp=x*sin(angle*!pi/180.) +y*cos(angle*!pi/180.)


;  interpolate dark image for time of observation

	if debug eq 1 then print,'interpolate dark image for time of observation'
	dark=dark_interp(date_dir,time,exposure)
	if debug eq 1 then print,'read dark image at:',time,' with exposure of:',exposure


	;  read flat images appropriate for these wavelengths and time

	if debug eq 1 then print,'read flat images appropriate for these wavelengths and time'

	read_flats,date_dir,wave,beam,time,flat,flat_header,flat_waves,flat_names,flat_expose
	flat=flat*expose/flat_expose  ;modify for exposure times


;  get image geometry from flat header

	occulter1 = {x:sxpar(flat_header,'OXCNTER1') - nx/2, y:sxpar(flat_header,'OYCNTER1') - 1024 + ny/2, r:sxpar(flat_header,'ORADIUS1')}
	occulter2 = {x:sxpar(flat_header,'OXCNTER2') - 1024 + nx/2, y:sxpar(flat_header,'OYCNTER2') - ny/2, r:sxpar(flat_header,'ORADIUS2')}

; Field position

	field1 = {x:sxpar(flat_header,'FXCNTER1') - nx/2, y:sxpar(flat_header,'FYCNTER1') - 1024 + ny/2, r:sxpar(flat_header,'FRADIUS1')}
	field2 = {x:sxpar(flat_header,'FXCNTER2') - 1024 + nx/2, y:sxpar(flat_header,'FYCNTER2') - ny/2, r:sxpar(flat_header,'FRADIUS2')}

;  get P angles of post from flat header

	post_angle1 = sxpar(flat_header,'POSTANG1')
	post_angle2 = sxpar(flat_header,'POSTANG2')

;  get overlap P angle (from the field stop)

	delta_x = sxpar(flat_header,'FXCNTER2') - sxpar(flat_header,'FXCNTER1')
	delta_y = sxpar(flat_header,'FYCNTER1') - sxpar(flat_header,'FYCNTER2')
	overlap_angle = !RADEG * atan(delta_y / delta_x)

;  get rid of all the blank comments in primary header

	sxdelpar,primary_header,'COMMENT'

;  add post P angle to primary header

	sxaddpar, primary_header, "POSTPANG", (post_angle1 + post_angle2) / 2., " [degrees] P Angle of occulter post"

;  add overlap P angle (from the field stop) to primary header

	sxaddpar, primary_header, "OVRLPANG", overlap_angle, " [degrees] P Angle of field overlap"

;  add ephemeris information to primary header

	sxaddpar,primary_header,'RSUN',semi_diam, " [arcsec] Solar Radius", format='(f8.2)'
	sxaddpar,primary_header,'SOLAR_P0',p_angle, " [degrees] P Angle", format='(f8.2)'
	sxaddpar,primary_header,'SOLAR_B0',b0, " [degrees] B Angle", format='(f8.2)'

 ;  add occulter (Sun center) parameters to primary header

	sxaddpar, primary_header, "CRPIX1", nx/2 + 0.5, " X [EAST->WEST ] SUN CENTER [PIXELS]"
	sxaddpar, primary_header, "CRVAL1", 0.0, " X [EAST->WEST ] SUN CENTER [ARCSEC]"
	sxaddpar, primary_header, "CDELT1", plate_scale, " solar_X coord increment [arcsec/pixel]"
	sxaddpar, primary_header, "CROTA1", 0.0, " X [EAST->WEST ] ROTATION [DEG.] FROM REFERENCE"

	sxaddpar, primary_header, "CRPIX2", ny/2 + 0.5, " Y [SOUTH->NORTH] SUN CENTER [PIXELS]"
	sxaddpar, primary_header, "CRVAL2", 0.0, " Y [SOUTH->NORTH] SUN CENTER [ARCSEC]"
	sxaddpar, primary_header, "CDELT2", plate_scale, " solar_Y coord increment [arcsec/pixel]"
	sxaddpar, primary_header, "CROTA2", 0.0, " Y [SOUTH->NORTH] ROTATION [DEG.] FROM REFERENCE"

  sxaddpar, primary_header, "ORADIUS", (occulter1.r + occulter2.r) / 2., " [pixels] Occulter Radius", format='(f8.2)'

;  add field parameters to primary header

;  center of field, offset from center of occulter and rotated by angle

	xoffset = ((field1.x + field2.x) / 2. - (occulter1.x + occulter2.x) / 2.)
	yoffset = ((field1.y + field2.y) / 2. - (occulter1.y + occulter2.y) / 2.)

    ; TODO is the following right?
	fxcent = nx / 2.0 + 0.5 + xoffset * cos(-p_angle*!pi/180.) - yoffset * sin(-p_angle*!pi/180.)
	fycent = ny / 2.0 + 0.5 + xoffset * sin(-p_angle*!pi/180.) + yoffset * cos(-p_angle*!pi/180.)
	sxaddpar, primary_header, "FRPIX1", fxcent, " [pixels] X [EAST->WEST ] FIELD CENTER [PIXELS]", format='(f8.2)'
	sxaddpar, primary_header, "FRPIX2", fycent, " [pixels] Y [SOUTH->NORTH] FIELD CENTER [PIXELS]", format='(f8.2)'
	sxaddpar, primary_header, "FRADIUS", (field1.r + field1.r) / 2., " [pixels] Field Radius", format='(f8.2)'

;  fix the date/time in UT in primary header

	fix_header_time, primary_header


;  compute mask

	comp_make_mask, date_dir, primary_header, mask
	good=where(mask eq 1.,complement=bad)


;  compute image offset matrices

	xpp1=xp+x0+occulter1.x
	ypp1=yp+y0+occulter1.y

	xpp2=xp+x0+occulter2.x
	ypp2=yp+y0+occulter2.y


;  open output fits file and write modified primary header to output file

	fits_open,out_file,fcb_out,/write       ;open output fits file
	fits_write, fcb_out, 0, primary_header


; >>>>>>>>>>>>>>>>>>>> loop over images <<<<<<<<<<<<<<<<<<<<

	for i=0,num_images-1 do begin

	  fits_read,fcb,dat,header,exten_no=i+1

	  if sxpar(header,'DEMULT') eq 0 then dat=demultiplex(dat)
	  dat=float(dat)-dark
	  dat=fixrock(dat,0.030)
	  dat=fix_image(dat)
	  fix_stray_light, dat, occulter1, occulter2, field1, field2, fit

	  iflat=where( flat_waves eq beam[i]*wave[i])			;  divide by flat field
	  dat=dat/flat[*,*,iflat[0]]

		dat=fix_hot(dat)			;  fix hot pixels

	  d1=extract1(dat)			;  extract sub-arrays
	  d2=extract2(dat)

	  d1=interpolate(d1,x1new,y1new,cubic=-0.5,missing=0.)			;  remove distortion
	  d2=interpolate(d2,x2new,y2new,cubic=-0.5,missing=0.)

	  d1=interpolate(d1,xpp1,ypp1,missing=0.,cubic=-0.5)
	  d2=interpolate(d2,xpp2,ypp2,missing=0.,cubic=-0.5)

		if beam[i] eq 1 then intensity=float(d2-d1) else intensity=float(d1-d2)

;		bad=where(intensity lt -50. or intensity gt 100.,count)
;		if count gt 0 then intensity[bad]=0.

		wset,2
    tv,bytscl(sqrt(intensity*mask),0,5)
    xyouts,0.35,0.45,/norm,string(beam[i]*wave[i])


;  clean up extension header before using it in writing

		sxdelpar,header,'COMMENT'
    sxdelpar,header,'SEQUENCE'
    sxdelpar,header,'LCVR1VOL'
    sxdelpar,header,'LCVR2VOL'
    sxdelpar,header,'LCVR3VOL'
    sxdelpar,header,'LCVR4VOL'
    sxdelpar,header,'LCVR5VOL'
    sxdelpar,header,'LCVR6VOL'
    sxdelpar,header,'LCVR1TMP'
    sxdelpar,header,'LCVR2TMP'
    sxdelpar,header,'LCVR3TMP'
  	sxdelpar,header,'LCVR4TMP'
  	sxdelpar,header,'LCVR5TMP'
  	sxdelpar,header,'LCVR6TMP'
  	sxdelpar,header,'BODYTEMP'
  	sxdelpar,header,'BASETEMP'
  	sxdelpar,header,'RACKTEMP'
  	sxdelpar,header,'OPTRTEMP'
  	sxdelpar,header,'FILTTEMP'

;  add inherit keyword to extension so that readers will get primary and extension headers merged
;  http://fits.gsfc.nasa.gov/registry/inherit.html

		sxaddpar,header,'INHERIT','T',AFTER='XTENSION'

	  fits_write,fcb_out,intensity,header

		if i mod 100 eq 0 then print,'processed image ',i

		wait,0.005

	endfor

	fits_close,fcb
	fits_close,fcb_out
	flush,2

endfor

print,'done'

end