; docformat = 'rst'

;+
; Setup for a reprocessing of a date.
;
; :Params:
;   date : in, required, type=string
;     date to process, in YYYYMMDD format
;-
pro comp_reprocess, date
  compile_opt strictarr
  @comp_config_common

  ; remove files from the archive
  ymd = comp_decompose_date(date)
  adir  = filepath('', subdir=ymd, root=archive_dir)
  frdir = filepath('', subdir=ymd, root=fullres_dir)
  mdir = filepath('', subdir=ymd, root=movie_dir)

  dirs = [adir, frdir, mdir]

  for d = 0L, n_elements(dirs) - 1L do begin
    if (file_test(dirs[d])) then begin
      mg_log, 'removing old files from...', dirs[d], name='comp', /info
      archived_files = file_search(filepath('*', root=dirs[d]), count=n_archived_files)
      
      if (n_archived_files gt 0L) then begin
        for f = 0L, n_archived_files - 1L do begin
          mg_file_delete, archived_files[f], status=error, message=message
          if (error ne 0L) then begin
            mg_log, 'error deleting %s', archived_files[f], name='comp', /error
            mg_log, message, name='comp', /error
            goto, done
          endif
        endfor
        mg_log, 'removed %d files from %s', n_old_files, dirs[d], $
                name='comp', /info
      endif else begin
        mg_log, 'no files to remove from %s', dirs[d], name='comp', /info
      endelse
    endif
  endfor

  done:
  mg_log, 'done', name='comp', /info
end
