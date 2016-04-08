; docformat = 'rst'

;+
; Distribute CoMP Level_1 files from processing pipeline into the appropriate
; directories.
;
; :Examples:
;   For example::
;
;     comp_distribute_l1, '20121209', '1074'
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :Author:
;   Sitongia  
;-
pro distribute_l1, date_dir, wave_type
  compile_opt strictarr
  @comp_config_common
  @comp_constants_common

  mg_log, 'distribute L1 for %s', wave_type, name='comp', /info
  
  process_dir = filepath('', subdir=date_dir, root=process_basedir)
  cd, process_dir
  
  ; for the directory name
  year = strmid(date_dir, 0, 4)
  month = strmid(date_dir, 4, 2)
  day = strmid(date_dir, 6, 4)
  destination = year + '/' + month + '/' + day  
  
  adir = filepath('', subdir=destination, root=archive_dir)
  frdir = filepath('', subdir=destination, root=fullres_dir), /overwrite

  ; prepare directories for level 1 files
  file_mkdir, adir
  file_chmod, adir, /g_write

  file_mkdir, frdir
  file_chmod, frdir, /g_write
  
  ; Copy ALL files
   
  ; copy FITS files to archive
  files = comp_find_l1_file(date_dir, wave_type, /all) + '.gz'
  file_copy, files, adir, /overwrite

  bkg_files = comp_find_l1_file(date_dir, wave_type, /all, /background) + '.gz'
  file_copy, bkg_files, adir, /overwrite
    
  ; copy FITS intensity files to archive
  files = file_search('*.comp.' + wave_type + '.intensity.gif', count=count)
  file_copy, files, frdir, /overwrite
  
  ; copy GOOD files
  
  files= 'good_' + wave_type + '_files.txt'   ; file with list of filenames
  n_files = file_lines(files)
  openr, lun, files, /get_lun
  line = ''
  for i = 0L, n_files - 1L do begin
    readf, lun, line
    rootname = strmid(line, 0, 15)
    
    file_copy, rootname + '.comp.' + wave_type + '.intensity.gif', frdir, /overwrite
  endfor
  
  free_lun, lun
 
  ; save the GBU file
  gbu_dir = filepath('', subdir=['GBU', year], root=log_dir)
  file_mkdir, gbu_dir
  file_copy, 'GBU.' + wave_type + '.log', $
             filepath(date_dir + '.GBU.' + wave_type + '.log', root=gbu_dir), $
             /overwrite
  
  ; tar and send to HPSS

  l1_tarname = date_dir + '.comp.' + wave_type + '.l1.tgz'

  mg_log, 'tar results', name='comp', /info
  ; TODO: check the performance of this command vs SPAWNing tar
  file_tar, file_search('*.comp.' + wave_type + '.fts.gz'), l1_tarname, /gzip
  ; spawn, 'tar cfz ' + l1_tarname + ' ' + '*.comp.' + wave_type + '.fts.gz'

  mg_log, 'send to HPSS', name='comp', /info
  file_link, filepath(tarname, root=process_dir), hpss_gateway

  mg_log, 'done', name='comp', /info
end
