pro enhance_int_comp, date_dir

common comp_constants, nx, ny, $
  center1074, center1079, center1083,  $
  stokes, n_stokes, debug, int_thresh, $
  diff_thresh

common comp_paths, bias_dir, flat_dir, mask_dir, binary_dir, $
  defered_file, hot_file, $
  ldm_basedir, raw_basedir, process_basedir, hpss_gateway, $
  archive_dir, movie_dir, fullres_dir, log_dir, $
  ffmpeg_dir, logo_dir

;common mask_constants, post_rotation, occulter_offset, field_offset, field_overlap, plate_scale

comp_initialize, date_dir
comp_paths, date_dir

window,1,xs=620,ys=620,xpos=100,ypos=100
window,2,xs=620,ys=620,xpos=750,ypos=100
window,3,xs=400,ys=300
window,4,xs=400,ys=300
window,5,xs=400,ys=300

process_dir = process_basedir + date_dir

files=process_dir+'/1074_files.txt'          ;file with list of filenames`
n_files = file_lines(files)           ;get number of filenames in file

openr,1,files
str=' '

filenames=strarr(n_files)

for j=0,n_files-1 do begin
	readf,1,str
	filenames[j]=strmid(str,0,15)
endfor
close,1      ;open file with filenames

for ifile=0,1 do begin

  name=process_dir+'/'+filenames[ifile]+'.l1.fts'    ;format input filename
  print,name

  fits_open,name,fcb        ;open input fits file
  num_images=fcb.nextend    ;find number of extensions

  inventory,fcb,beam,group,wave,pol,type,expose,cover,cal_pol,cal_ret

  uniq_beam=beam[uniq(beam,sort(beam))]	;find unique beams and wavelengths
  uniq_wave=wave[uniq(wave,sort(wave))]
  nbeam=n_elements(uniq_beam)
  nwave=n_elements(uniq_wave)

  ;  read primary header

  fits_read,fcb,d,primary_header,/header_only,exten_no=0
  index = convert_ascii_header(primary_header)

  n=nwave*nbeam     ;number of images to combine (2 or 4)

  num_images=num_images - (num_images mod n)     ;insure number of images is divisible by n

  comp_make_mask2,date_dir,primary_header,mask,occ_fac=1.05,fld_fac=0.95
  mask=mask_top_bottom(620,620,100)*mask

  nextend=fcb.nextend

  x_offset=fltarr(nextend/n)
  y_offset=fltarr(nextend/n)
  theta_offset=fltarr(nextend/n)

  for i=0,num_images-1,n do begin

    fits_read, fcb, blue1, header1, exten_no=i+1
    fits_read, fcb, red1, header2, exten_no=i+2

    if nbeam eq 2 then begin
      fits_read, fcb, blue2, header3, exten_no=i+3
      fits_read, fcb, red2, header4, exten_no=i+4

      blue=(blue1+blue2)/2.
		  red=(red1+red2)/2.
    ENDIF

    blue=comp_intensity_enhancement(blue, primary_header)
    red=comp_intensity_enhancement(red, primary_header)

    wset,1
    tvscl,bytscl(blue)

    wset,2
    tvscl,bytscl(red)

    coalign_parms=comp_coregister2(blue, red, index, index, mask, guess=guess)
    print,i,coalign_parms


    x_offset[i/n]=coalign_parms[0]    ;save offsets for plotting
    y_offset[i/n]=coalign_parms[1]
    theta_offset[i/n]=coalign_parms[2]

  endfor

  fits_close,fcb_in
  wset,3        ;plot offsets
  plot,x_offset

  wset,4
  plot,y_offset

  wset,5
  plot,theta_offset

  save,file=string(process_dir+'/'+filenames[ifile]+'.ca_br.sav'),x_offset,y_offset,theta_offset,coalign_parms
endfor

close,1

;printf, logFileUnit, "done ", systime()
;close, logFileUnit
;free_lun, logFileUnit

print,'done'

end
