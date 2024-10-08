;+
;  Name: coalign_br
;
;  Description:
;    Procedure to apply offsets computed with find_offsets_fastdop to CoMP fast doppler datasets
;    which will align them with AIA images.
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
;    coalign_fastdop, '20140123'
;
;  Author: Plowman
;  Modified by: Tomczyk
;-
pro coalign_br, date_dir, list_file = list_file

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

window,0,xs=600,ys=470,retain=2,xpos=0,ypos=0,title='x-offset'
window,1,xs=600,ys=470,retain=2,xpos=0,ypos=510,title='y-offset'
window,2,xs=600,ys=470,retain=2,xpos=0,ypos=1020,title='theta-offset'
device,decomposed=0

process_dir = process_basedir + date_dir        ;processing directory

; Logging
log_file = log_dir + 'cidx/' + date_dir+ '.log'
get_lun, logFileUnit
openw, logFileUnit, log_file, /APPEND

printf, logFileUnit, 'coalign_fastdop ', systime()
print, date_dir

; Establish error handler. When errors occur, the index of the
; error is returned in the variable Error_status:
CATCH, Error_status

;This statement begins the error handler:
IF Error_status NE 0 THEN BEGIN
  printf, logFileUnit, 'coalign_fastdop: Error message: ', !ERROR_STATE.MSG
  print, 'coalign_fastdop: Error message: ', !ERROR_STATE.MSG
  CATCH, /CANCEL
  close, logFileUnit
  free_lun, logFileUnit
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

printf, logFileUnit, 'Using files from ' + files
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

for ifile=0,7 do begin
;for ifile=0,0 do begin

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
  fits_read,fcb_in,data1,s_header,exten_no=1

  sxaddpar,p_header,'CRVAL1', float(0)
  sxaddpar,p_header,'CRVAL2', float(0)
  sxaddpar,p_header,'CROTA2', float(0)

	index = convert_ascii_header(p_header) ; Make index structure

  nextend=fcb_in.nextend     ;get number of image extensions

;  create output fits file (with .ca extension)

  out_name=process_dir+'/'+filenames[ifile]+'.ca_br.1.fts'
  fits_open,out_name,fcb_out,/write
  fits_write,fcb_out,0,p_header     ;copy primary header to output fits file


;  loop over all extensions and align each image with same aia image

  restore,file=string(process_dir+'/'+filenames[ifile]+'.ca_br.sav')   ;restore offsets

  num=n_elements(x_offset)*n    ;number of images to process

;  fit low order polynomial to data and plot
  xa=indgen(n_elements(x_offset))

  wset,0
  plot,x_offset
  p1=poly_fit(xa,x_offset,4)
  x_apply=p1[0]+p1[1]*xa+p1[2]*xa*xa+p1[3]*xa*xa*xa+p1[4]*xa*xa*xa*xa

  wset,1
  plot,y_offset
  p2=poly_fit(xa,y_offset,4)
  y_apply=p1[0]+p1[1]*xa+p1[2]*xa*xa+p1[3]*xa*xa*xa+p1[4]*xa*xa*xa*xa

  wset,2
  plot,theta_offset
  theta_apply=median(theta_offset)
  oplot,[0.,num],[theta_apply,theta_offset]

  num_images=nextend - (nextend mod n)     ;make number of images is divisible by n
  for i=0,num-1 do begin
  ;for i=0,60 do begin

    fits_read, fcb_in, data, header, exten_no=i+1
    if (i mod 4 eq 0) or (i mod 4 eq 2) then begin
      sxaddpar,header,'CRVAL1', float(0)
      sxaddpar,header,'CRVAL2', float(0)
      sxaddpar,header,'CROTA2', float(0)

      fits_write,fcb_out,data,header

    endif else begin

      ; sxaddpar,header,'CRVAL1', float(x_apply[i/4]-mean(x_apply))
      ; sxaddpar,header,'CRVAL2', float(y_apply[i/4]-mean(y_apply))
      ; sxaddpar,header,'CROTA2', float(theta_apply)
      ;
      ; coalign_parms=[x_apply[i/4]-mean(x_apply),y_apply[i/4]-mean(y_apply),theta_apply]

      sxaddpar,header,'CRVAL1', float(x_offset[i/4]-mean(x_offset))
      sxaddpar,header,'CRVAL2', float(y_offset[i/4]-mean(y_offset))
      sxaddpar,header,'CROTA2', float(theta_apply)



  ;  resample image
      coalign_parms=[x_offset[i/4]-mean(x_offset),y_offset[i/4]-mean(y_offset),theta_apply]

      data_resamp = comp_coreg_resamp(data, index, coalign_parms)

      fits_write,fcb_out,data_resamp,header     ;write resampled image to output file

    endelse


;  add x, y, and theta offsets to extension header

    ; sxaddpar,header,'CRVAL1', float(x_offset[i/4]-mean(x_offset))
    ; sxaddpar,header,'CRVAL2', float(y_offset[i/4]-mean(y_offset))
    ; sxaddpar,header,'CROTA2', float(theta_apply)



;  resample image
    ;coalign_parms=[x_offset[i/4]-mean(x_offset),y_offset[i/4]-mean(y_offset),theta_apply]


  endfor

  fits_close,fcb_in
  fits_close,fcb_out

endfor

close,1

printf, logFileUnit, "done ", systime()
close, logFileUnit
free_lun, logFileUnit
free_lun,1

print,'done'

end
