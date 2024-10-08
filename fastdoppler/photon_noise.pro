;+
;  Name: photon_noise
;  
;  Description:
;    procedure to compute photon noise from mean image
;
;  Inputs:
;    date_dir - day of year to process, in YYYYMMDD format
;    wave_type - wavelength range for the observations, '1074', '1079' or '1083'
;
;  Keywords: none
;
;  Author: sitongia
;  Modified by: Tomczyk
; 
;  Example: 
;    photon_noise, '20130915', '1074'   
;-
pro photon_noise, date_dir, wave_type
    
  common comp_constants, nx, ny, $
    center1074, center1079, center1083,  $
    stokes, n_stokes, debug, int_thresh, $
    diff_thresh

  common comp_paths, bias_dir, flat_dir, mask_dir, binary_dir, $
    defered_file, hot_file, $
    ldm_basedir, raw_basedir, process_basedir, hpss_gateway, $
    archive_dir, movie_dir, fullres_dir, log_dir, $
    ffmpeg_dir, logo_dir

  COMPILE_OPT IDL2

  ; Configure
  comp_initialize, date_dir
  comp_paths, date_dir
  
  ; Logging
  log_file = log_dir + 'cidx/' + date_dir+ '.log'
  get_lun, logFileUnit
  openw, logFileUnit, log_file, /APPEND
  
  printf, logFileUnit, 'photon_noise ' + wave_type, systime()
  
  ; Establish error handler. When errors occur, the index of the
  ; error is returned in the variable Error_status:
  CATCH, Error_status
  
  ;This statement begins the error handler:
  IF Error_status NE 0 THEN BEGIN
    ; why the heck won't IDL print the following to the logFileUnit???
    printf, logFileUnit, 'photon_noise: Error message: ', !ERROR_STATE.MSG
    CATCH, /CANCEL
    close, logFileUnit
    free_lun, logFileUnit
    return
  ENDIF
  
  process_dir = process_basedir + date_dir

  cd, process_dir
  
  ; Constant: photons per millionth
  kc = 875.0
  
  file=date_dir+'.comp.'+wave_type+'.mean.fts'
  fits_open,file,fcb
  n=fcb.nextend
  
  ; Copy the primary header from the mean file to the output file
  fits_read,fcb,d,primary_header,/header_only,exten_no=0  

  ntune=sxpar(primary_header,'NTUNE', count=nrecords)
  if nrecords eq 0 then ntune=sxpar(primary_header,'NTUNES')
  
  nstokes=fix(n/ntune)-1    ;compute number of stokes parameters assuming background is included
  center_index = ntune/2
  
  ;  read central intensity, number of frames averaged and polarization states

  fits_read,fcb,intens,header,exten_no=center_index+1
  nframes=intarr(nstokes)
  pol=strarr(nstokes)
  
  for i=0,nstokes-1 do begin
    fits_read,fcb,d,header,/header_only,exten_no=i*ntune+center_index+1
    nframes[i]=sxpar(header, 'NAVERAGE')
    pol[i]=strcompress(sxpar(header,'POLSTATE'),/remove_all)  
  endfor
     
  ; read background level
  
  fits_read,fcb,b,header,exten_no=ntune*nstokes+center_index+1
  fits_close,fcb
  
  spawn, "svnversion /home/sitongia/projects/CoMP/Pipeline", version
  sxaddpar, primary_header, 'VERSION', version[0], ' Software Subversion Revision'
  
  noise = sqrt(intens + b) / sqrt(kc)
  
  ;  write fit parameters to output file
    
  fits_open,date_dir+'.comp.'+wave_type+'.photon_noise.fts',fcbout,/write

  ; Copy the primary header from the mean file to the output file
  fits_write, fcbout, 0, primary_header

  sxdelpar,header,'WAVELENG'
  sxdelpar,header,'DATATYPE'
  sxdelpar,header,'FILTER'
  sxdelpar,header,'COMMENT'
  sxaddpar,header,'NTUNE',ntune
  sxaddpar,header,'LEVEL   ','L2'
  
  for i=0,nstokes-1 do begin
    sxaddpar,header,'POLSTATE',pol[i]
    sxaddpar,header,'NAVERAGE', nframes[i]
    noise_s= noise / sqrt(float(nframes[i]))
    str='Noise '+pol[i]
    fits_write,fcbout,noise_s,header,extname=str   
  endfor  
  
  fits_close,fcbout  
  
  ; For the directory name
  year = strmid(date_dir, 0, 4)
  month = strmid(date_dir, 4, 2)
  day = strmid(date_dir, 6, 4)
  destination = year + "/" + month + "/" + day  

  ; Compress files
  if debug eq 1 then print, "Compressing FITS files."
  ;spawn, 'gzip -f ' + date_dir+'.comp.'+wave_type+'.photon_noise.fts'
    
  ; Copy FITS files to
  ;file_copy, date_dir+'.comp.'+wave_type+'.photon_noise.fts.gz', archive_dir + destination, /OVERWRITE
  
  printf, logFileUnit, "done ", systime()
  close, logFileUnit
  free_lun, logFileUnit
  
end