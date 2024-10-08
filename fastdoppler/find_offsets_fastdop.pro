;+
;  Name: find_offsets_fastdop
;
;  Description:
;    Procedure to read CoMP fast doppler datasets and compute offsets in x, y, and theta that
;    align the images with AIA images.
;
;  Input Parameters:
;    date_dir - date to process, in YYYYMMDD format
;
;  Keyword Parameters:
;    list_file - optional filename containing list of files to process
;
;  Output:
;    New FITS file containing aligned CoMP images (optional)
;
;  Examples:
;    find_offsets_fastdop, '20140123'
;
;  Author: Plowman
;  Modified by: Tomczyk
;-
pro find_offsets_fastdop, date_dir, list_file = list_file

common comp_paths, bias_dir, flat_dir, mask_dir, binary_dir, $
  defered_file, hot_file, $
  ldm_basedir, raw_basedir, process_basedir, hpss_gateway, $
  archive_dir, movie_dir, fullres_dir, log_dir, $
  ffmpeg_dir, logo_dir

common comp_constants, nx, ny, $
  center1074, center1079, center1083,  $
  stokes, n_stokes, debug, int_thresh, $
  diff_thresh

COMPILE_OPT IDL2

; Configure

comp_paths, date_dir
comp_initialize, date_dir

devicelib
imagelib

;  create windows

window,0,xs=600,ys=470,retain=2,xpos=1900,ypos=0,title='x-offset'
window,1,xs=600,ys=470,retain=2,xpos=1900,ypos=510,title='y-offset'
window,2,xs=600,ys=470,retain=2,xpos=1900,ypos=1020,title='theta-offset'
device,decomposed=0

aiadir = '/Data/wave/AIAframes/'
;aiadir = '/Volumes/E/wave/AIAframes/'
process_dir = process_basedir + date_dir        ;processing directory

; Logging
;log_file = log_dir + 'cidx/' + date_dir+ '.log'
;get_lun, logFileUnit
;openw, logFileUnit, log_file, /APPEND

;printf, logFileUnit, 'coalign_fastdop ', systime()
print, date_dir

; Establish error handler. When errors occur, the index of the
; error is returned in the variable Error_status:
CATCH, Error_status

;This statement begins the error handler:
IF Error_status NE 0 THEN BEGIN
;  printf, logFileUnit, 'coalign_fastdop: Error message: ', !ERROR_STATE.MSG
  print, 'coalign_fastdop: Error message: ', !ERROR_STATE.MSG
  CATCH, /CANCEL
;  close, logFileUnit
;  free_lun, logFileUnit
  return
ENDIF

; Check keywords
uselist = 0
if n_elements(list_file) eq 1 then uselist = 1

process_dir = process_basedir + date_dir

cd, process_dir

!except=0


if (uselist) then begin
	files = list_file
	n_files = file_lines(list_file)
endif else begin
 	files=process_dir+'/1074_files.txt'   ;file with list of filenames`
 	n_files = file_lines(files)
endelse


;	read list of filenames

;printf, logFileUnit, 'Using files from ' + files
openr,1,files
str=' '

filenames=strarr(n_files)

for j=0,n_files-1 do begin
	readf,1,str
	filenames[j]=strmid(str,0,15)
endfor
close,1


;	***** loop over all files in list *****

openw,1,'log.txt'

;for ifile=0,0 do begin
for ifile=0, n_files-1 do begin

	name=process_dir+'/'+filenames[ifile]+'.l1.fts'
	print,name
	fits_open,name,fcb_in                  ;open input file

;  take inventory of input file

	inventory,fcb_in,beam,group,wave,pol,type,expose,cover,cal_pol,cal_ret

	uniq_beam=beam[uniq(beam,sort(beam))] ;find unique beams and wavelengths
	uniq_wave=wave[uniq(wave,sort(wave))]
	nbeam=n_elements(uniq_beam)
	nwave=n_elements(uniq_wave)
  n=nbeam*nwave

;	read the primary data and header and put contents of primary header into structure

	fits_read,fcb_in,d,p_header,/header_only,exten_no=0
	index = convert_ascii_header(p_header) ; Make index structure

  nextend=fcb_in.nextend     ;get number of image extensions

; Make mask for coalignment. Heavily cropped to avoid artifacts near disk or edge:
; Compute just one for each file.

  comp_make_mask2,date_dir,p_header,mask,occ_fac=1.1,fld_fac=0.95
  mask=mask_top_bottom(620,620,100)*mask

;  get aia image appropriate for this file

  date = index.dateobs
  time = index.timeobs

  ccsdstime = date+'T'+time+'.500Z'

  aiasearch = vso_search(near=ccsdstime,inst='aia',wave='211 Angstrom',/quiet)
  a=vso_get(aiasearch, out_dir=aiadir, filenames=aiafile,/quiet)
  aia_prep,aiafile,[0],aiaindex,aiadata,/uncomp_delete


;  loop over all extensions and align each image with same aia image

  x_offset=fltarr(nextend/n)
  y_offset=fltarr(nextend/n)
  theta_offset=fltarr(nextend/n)

  num_images=nextend - (nextend mod n)     ;make number of images is divisible by n

;  read in red and blue images (2 if one beam, 4 if two beam)

  ;for i=0,60,n do begin
  for i=0,num_images-1,n do begin

    fits_read, fcb_in, data1, header1, exten_no=i+1
    fits_read, fcb_in, data2, header2, exten_no=i+2

    if nbeam eq 2 then begin
      fits_read, fcb_in, data3, header3, exten_no=i+3
      fits_read, fcb_in, data4, header4, exten_no=i+4

      image=(data1+data2+data3+data4)/4.
    endif else image=(data1+data2)/2.

    coalign_parms=comp_coregister(aiadata, image, aiaindex, index, mask, guess=guess)
    print,i,coalign_parms
    printf,1,i,coalign_parms

    x_offset[i/n]=coalign_parms[0]    ;save offsets for plotting
    y_offset[i/n]=coalign_parms[1]
    theta_offset[i/n]=coalign_parms[2]

  endfor

  fits_close,fcb_in

  wset,0        ;plot offsets
  plot,x_offset

  wset,1
  plot,y_offset

  wset,2
  plot,theta_offset

  save,file=string(process_dir+'/'+filenames[ifile]+'.ca.sav'),x_offset,y_offset,theta_offset,coalign_parms
endfor

close,1

;printf, logFileUnit, "done ", systime()
;close, logFileUnit
;free_lun, logFileUnit

print,'done'

end
