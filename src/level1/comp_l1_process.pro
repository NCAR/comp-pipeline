; docformat = 'rst'

;+
; Do the level 1 processing for all the files on a certain day of a specific
; wavelength.
;
; :Uses:
;   comp_constants_common, comp_config_common, comp_mask_constants_common,
;   comp_initialize, comp_configuration, comp_l1_process_file, comp_ut_filename
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength to process, '1074', '1079', etc.
;
; :Keywords:
;   error : out, optional, type=long
;     set to a named variable to retrieve an error code; 0 indicates no error
;
; :Author:
;   MLSO Software Team
;-
pro comp_l1_process, date_dir, wave_type, error=error
  compile_opt strictarr

  @comp_constants_common
  @comp_config_common
  @comp_mask_constants_common

  mg_log, 'wave type: %s', wave_type, name='comp', /info

  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    mg_log, /last_error, name='comp'
    return
  endif

  raw_dir = filepath(date_dir, root=raw_basedir)
  process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)

  cd, process_dir  ; change directory to processing directory

  ; file with list of file names
  infiles = filepath(string(date_dir, wave_type, $
                            format='(%"%s.comp.%s.files.txt")'), $
                     root=process_dir)
  extens = wave_type   ; entension for files

  ; synoptic processing works on a subset of files in the morning
  n_file_lines = file_lines(infiles)

  openr, infiles_lun, infiles, /get_lun

  in_filename = ''

  fmt = string(floor(alog10(n_file_lines)) + 1L, format='(%"%%%dd/%%d @ %%s: %%s")')

  for file_count = 0L, n_file_lines - 1L do begin
    readf, infiles_lun, in_filename, format='(a19)'
    mg_log, fmt, file_count + 1L, n_file_lines, wave_type, in_filename, $
            name='comp', /info
    filename = filepath(in_filename, subdir=date_dir, root=raw_basedir)
    comp_l1_process_file, filename, date_dir, wave_type
  endfor

  free_lun, infiles_lun

  ; zip L1 (and background) files
  mg_log, 'zipping L1 files...', name='comp', /info
  file_pattern = string(wave_type, format='(%"*.*.comp.%s.*.fts")')
  l1_files = file_search(file_pattern, count=n_l1_files)
  if (n_l1_files gt 0L) then begin
    mg_log, 'zipping %d %s files', n_l1_files, wave_type, name='comp', /info
    zip_cmd = string(file_pattern, $
                     format='(%"gzip -f %s")')
    spawn, zip_cmd, result, error_result, exit_status=status
    if (status ne 0L) then begin
      mg_log, 'problem zipping files with command: %s', zip_cmd, $
              name='comp', /error
      mg_log, '%s', error_result, name='comp', /error
    endif
  endif else begin
    mg_log, 'no %s files to zip', wave_type, name='comp', /info
  endelse

  mg_log, 'done', name='comp', /info
end
