; docformat = 'rst'

;+
; Find the full path to a L1 file given a date/time.
;
; :Returns:
;   string
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   datetime : in, required, type=string
;     date/time in the form "20150701.220501"
;   wave_type : in, required, type=string
;     wavelength type such as "1074", "1079", or "1083"
;
; :Keywords:
;   background : in, optional, type=boolean
;     set to retrieve a background image instead of a foreground image
;-
function comp_find_l1_file, date, datetime, wave_type, background=background
  compile_opt strictarr
  @comp_config_common

  filename_pattern = datetime + '.comp.' + wave_type + '*.*.fts'
  filenames = file_search(filepath(filename_pattern, $
                                   subdir=date, $
                                   root=process_basedir), $
                          count=n_files)

  background_mask = strmatch(file_basename(filenames), '*.bkg.fts')
  background_ind = where(background_mask, complement=foreground_ind)

  return, filenames[keyword_set(background) ? background_ind[0] : foreground_ind[0]]
end
