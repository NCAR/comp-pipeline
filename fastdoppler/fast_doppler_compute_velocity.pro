;+
;  Name: fast_doppler_compute_velocity
;
;  Description:
;    Procedure to compute fast cadence doppler images and write them to a fits file. This
;    procedure will work on either one or two beam fast doppler data (two wavelengths only).
;    A velocity image is computed from either two images (one beam) or four images (two beam)
;    using an algorithm which assumes the line profile is gaussian and the linewidth is 0.14 nm.
;
;  Input Parameters:
;    date_dir - date to process, in YYYYMMDD format
;
;  Keyword Parameters: none
;
;  Output:
;    The computed intensity and velocity images are written sequentially to a fits file with
;    one extension for each image.
;
;  Author: Tomczyk
;
;  Examples:
;    fast_doppler_compute_velocity, '20140102'
;-
pro fast_doppler_compute_velocity, date_dir

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

comp_initialize, date_dir
comp_paths, date_dir

str=' '
debug=0

if debug eq 1 then begin
  window,0,xs=nx,ys=ny,title='Velocity'
  window,2,xs=nx,ys=ny,title='Blue Intensity'
  window,3,xs=nx,ys=ny,xpos=0,ypos=650,title='Red Intensity'
endif

process_dir = process_basedir + date_dir

filenames=process_dir+'/1074_files.txt'          ;file with list of filenames`
n_files = file_lines(filenames)           ;get number of filenames in file

openr,1,filenames       ;open file with filenames

for ifile=0,n_files-1 do begin
  readf,1,str

  name=process_dir+'/'+strmid(str,0,15)+'.ca.fts'    ;format input filename
  print,name

  fits_open,name,fcb        ;open input fits file
  num_images=fcb.nextend    ;find number of extensions

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

  n=nwave*nbeam     ;number of images to combine (2 or 4)

  num_images=num_images - (num_images mod n)     ;insure number of images is divisible by n

;  open output fits file

  out_file=process_dir+'/'+strmid(str,0,15)+'.vel.fts'    ;format input filename
  fits_open,out_file,fcbout,/write

  sxaddpar,primary_header,'NAVERAGE',n    ;add naverage to extension header

; Copy the primary header from the input file to the output file

  fits_write,fcbout,0,primary_header

  for i=0,num_images-1,n do begin    ;--------- loop over all images in input file ----------

    fits_read,fcb,blue,header,exten_no=i+1
    fits_read,fcb,red,header,exten_no=i+2

	  if nbeam gt 1 then begin		;read other beam if it is there
	    fits_read,fcb,blue2,header,exten_no=i+3
    	fits_read,fcb,red2,header,exten_no=i+4

		  blue=(blue+blue2)/2.
		  red=(red+red2)/2.
	  endif

	  vel=const*alog(red/blue)		;compute velocity assuming constant line width
    good=where(finite(vel),complement=bad,count)
    if count gt 0 then vel[bad]=0.

    intens=(red+blue)/2.
    good=where(finite(intens),complement=bad,count)
    if count gt 0 then vel[bad]=0.

	  sxdelpar,header,'WAVELENG'   ;remove some information from extension header
	  sxdelpar,header,'POLSTATE'
	  sxdelpar,header,'FILTER'
	  sxdelpar,header,'BEAM'

    sxaddpar,header,'DATATYPE','INTENSTY'    ;add naverage to extension header
    fits_write,fcbout,intens,header    ;write intensity image
    sxaddpar,header,'DATATYPE','VELOCITY'    ;add naverage to extension header
    fits_write,fcbout,vel,header              ;write velocity image

;  display red, blue and velocity images

    if debug eq 1 then begin
      wset,2
      tv,bytscl(blue,0.,25.)

      wset,3
      tv,bytscl(red,0.,25.)

      wset,0
      bvel=bytscl(vel,-10,10)
      tv,bvel
    endif

  endfor

  fits_close,fcb
  fits_close,fcbout

endfor

close,1

print,'done'

end
