; docformat = 'rst'

;+
; Create an mp4 of the science images that didn't pass quality.
;
; :Params:
;   date : in, required, type=string
;     date in the form "YYYYMMDD"
;-
pro comp_create_bad_mp4, date
  compile_opt strictarr
  @comp_common_config

  ; find .bad.gifs in engineering directory
  bad_gifs = file_search(filepath('*.bad.gif', $
                                  subdir=comp_decompose_date(date), $
                                  root=engineering_basedir), $
                         count=n_bad_gifs)
  if (n_bad_gifs eq 0L) then begin
    mg_log, 'no bad science images, exiting', name='comp', /info
    goto, done
  endif else begin
    mg_log, 'creating mp4 of %d bad science images', name='comp', /info
  endelse

  ; sort list of frames
  ind = sort(file_basename(bad_gifs))
  bad_gifs = bad_gifs[ind]

  ; create mp4
  bad_mp4_filename = filepath(string(date, format='(%"%s.bad.mp4")'), $
                              subdir=comp_decompose_date(date), $
                              root=engineering_basedir)
  comp_create_mp4, bad_gifs, bad_mp4_filename, $$
                   executable=filepath('ffmpeg', root=ffmpeg_dir), $
                   frames_per_second=2L, $
                   status=status, $
                   error_message=error_message

  if (status ne 0L) then begin
    mg_log, 'error %d creating mp4 of bad science images', status, name='comp', /error
    for i = 0L, n_elements(error_message) - 1L do begin
      mg_log, error_message[i], name='comp', /error
    endfor
  endif else begin
    mg_log, 'mp4 of bad science images %s', bad_mp4_filename, name='comp', /info
  endelse

  done:
  mg_log, 'done', name='comp', /info
end
