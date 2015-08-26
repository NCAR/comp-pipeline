; docformat = 'rst'

;+
; Change filenames in CoMP inventory files from local time to UT. 
;
; Output is new versions of the files: 1074_files.txt, 1079_files.txt,
; 1083_files.txt with the times in the filenames now corresponding to UT.    
;  
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;  
; :Examples:
;   For example, to update the inventory files for '20121209'::
;
;     comp_update_filenames, '20121209'
;
; :Uses:
;   strput
;
; :Author:
;   sitongia
;-
pro comp_update_filenames, date_dir
  compile_opt idl2
  @comp_paths_common

  ; configure
  comp_paths, date_dir
  process_dir = filepath(date_dir, root=process_basedir)
  cd, process_dir

  old_filenames = ['1074_files.txt', '1079_files.txt', '1083_files.txt']

  str = ''
  for i = 0L, n_elements(old_filenames) - 1L do begin
    new_filename = 'new_' + old_filenames[i]
    openr, old_lun, old_filenames[i], /get_lun
    openw, new_lun, new_filename, /get_lun
    while (not eof(old_lun)) do begin
      readf, old_lun, str
      strput, str, comp_ut_filename(strmid(str, 0, 15)), 0
      printf, new_lun, str
    endwhile
    free_lun, old_lun
    free_lun, new_lun

    file_move, new_filename, old_filenames[i], /overwrite
  endfor
end
