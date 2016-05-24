; docformat = 'rst'

;+
; Lock/unlock a raw directory.
;
; :Returns:
;   1 if lock/unlock successful, 0 if not
;
; :Params:
;   date_dir : in, required, type=string
;     day of year to process, in `YYYYMMDD` format
;
; :Keywords:
;   lock : in, optional, type=boolean
;     set to try to obtain a lock on the `date_dir` in the raw
;     directory
;   unlock : in, optional, type=boolean
;     set to unlock a `date_dir` in the raw directory
;   processed : in, optional, type=boolean
;     set to set a lock indicating the directory has been processed
;-
function comp_state, date_dir, lock=lock, unlock=unlock, processed=processed
  compile_opt strictarr, logical_predicate
  on_error, 2
  @comp_config_common

  raw_dir = filepath(date_dir, root=raw_basedir)
  lock_file = filepath('.lock', root=raw_dir)
  processed_file = filepath('.processed', root=raw_dir)

  if (keyword_set(lock)) then begin
    available = ~file_test(lock_file) && ~file_test(processed_file)
    if (available) then begin
      openw, lun, lock_file, /get_lun
      free_lun, lun
    endif
    return, available
  endif

  if (keyword_set(unlock)) then begin
    locked = file_test(lock_file)
    if (locked) then begin
      file_delete, lock_file
    endif
    return, locked
  endif

  if (keyword_set(processed)) then begin
    openw, lun, processed_file, /get_lun
    free_lun, lun
    return, 1B
  endif
end
