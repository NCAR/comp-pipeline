; docformat = 'rst'

;+
; Procedure to interpolate a dark image for time of observation. File
; containing all darks for that day is used.
;
; :Uses:
;   comp_constants_common, comp_paths_common, fits_open, fits_read, fits_close,
;   sxpar, mglog
;
; :Returns:
;   bias
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;    time
;    exposure
;
; :Author:
;   Tomczyk
;-
function comp_dark_interp, date_dir, time, exposure
  compile_opt idl2
  @comp_constants_common
  @comp_paths_common

  process_dir = filepath(date_dir, root=process_basedir)

  ; open output fits file
  fits_open, filepath('dark.fts', root=process_dir), fcb

  ; read arrays with times and exposures
  num = fcb.nextend
  fits_read, fcb, times, exten_no=num - 1L
  fits_read, fcb, exposures, exten_no=num

  good = where(exposures eq exposure, count)

  if (count le 0L) then begin
    mg_log, 'no darks for exposure: %s', strtrim(exposure, 2), name='comp', /error
    message, 'No darks for exposure ' + strtrim(exposure, 2)
  endif

  ; check for time beyond endpoints
  if (time le min(times[good])) then begin   ; time before first bias
    fits_read, fcb, bias, exten_no=good[0] + 1
    mg_log, '%d at %f', good[0], times[good[0]], name='comp/dark_interp', /debug
  endif else begin
    if (time ge max(times[good])) then begin   ; time after last bias
      fits_read, fcb, bias, exten_no=good[count-1] + 1
      mg_log, '%d at %f', good[count - 1L], times[good[count - 1L]], $
              name='comp/dark_interp', /debug
    endif else begin
      ;  otherwise interpolate for time
      closest = min(abs(times[good] - time), tmin)
      time_diff = times[good[tmin]] - time
      if (time_diff le 0) then begin
        i1 = good[tmin]
        i2 = good[tmin + 1L]
      endif else begin
        i1 = good[tmin - 1L]
        i2 = good[tmin]
      endelse

      mg_log, 'times[good] = %s', strjoin(strtrim(times[good], 2), ', '), $
              name='comp/dark_interp', /debug
      mg_log, 'tmin = %f at %f, %f', tmin, times[i1], times[i2], $
              name='comp/dark_interp', /debug

      fits_read, fcb, bias1, header1, exten_no=i1 + 1
      fits_read, fcb, bias2, header2, exten_no=i2 + 1

      t1 = sxpar(header1, 'TIME')
      t2 = sxpar(header2, 'TIME')
      mg_log, 't1 = %f, t2 = %f', t1, t2, name='comp/dark_interp', /debug

      f1 = (times[i2] - time) / (times[i2] - times[i1])
      f2 = (time - times[i1]) / (times[i2] - times[i1])

      mg_log, 'f1 = %f, f2 = %f', f1, f2, name='comp/dark_interp', /debug

      bias = f1 * bias1 + f2 * bias2
    endelse
  endelse

  fits_close, fcb
  return, bias
end
