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
;-
pro comp_l1_process, date_dir, wave_type, error=error
  compile_opt strictarr

  @comp_constants_common
  @comp_config_common
  @comp_mask_constants_common

  ; configure
  comp_initialize, date_dir
  comp_configuration

  mg_log, 'wave_type: %s', wave_type, name='comp', /info

  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    mg_log, /last_error, name='comp'
    return
  endif

  raw_dir = filepath(date_dir, root=raw_basedir)
  process_dir = filepath(date_dir, root=process_basedir)

  cd, process_dir  ; change directory to processing directory

  ; file with list of file names
  infiles = filepath(wave_type + '_files.txt', root=process_dir)
  extens = wave_type   ; entension for files

  ; synoptic processing works on a subset of files in the morning
  n_file_lines = file_lines(infiles)

  openr, infiles_lun, infiles, /get_lun

  in_filename = ''

  for file_count = 0L, n_file_lines - 1L do begin
    readf, infiles_lun, in_filename, format='(a19)'
    mg_log, 'processing data file (%d/%d for %s): %s', $
            file_count + 1L, n_file_lines, wave_type, in_filename, $
            name='comp/l1_process', /info
    datetime = strmid(in_filename, 0, 15)
    in_filename = filepath(in_filename, subdir=date_dir, root=raw_basedir)
    out_filename = filepath(string(comp_ut_filename(datetime), $
                                   extens, $
                                   format='(%"%s.comp.%s.fts")'), $
                            root=process_dir)
    comp_l1_process_file, in_filename, out_filename, date_dir
  endfor

  free_lun, infiles_lun

  mg_log, 'done', name='comp', /info
end
