; docformat = 'rst'

;+
; Check various metrics in final L1 files and send notifications if needed.
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, optional, type=string
;     wavelength range for the observations, '1074', '1079' or '1083';
;     distribute wavelength independent files such as flats and darks if not
;     provided
;-
pro comp_l1_check, date_dir, wave_type
  compile_opt strictarr
  @comp_constants_common
  @comp_config_common

  l1_files = comp_find_l1_file(date_dir, wave_type, /all, count=n_l1_files)

  overlap_angle_warning = 0B

  for f = 0L, n_l1_files - 1L do begin
    mg_log, 'checking %s', file_basename(l1_files[f]), name='comp', /info

    ; check overlap angle deviation from 45 degrees
    fits_open, l1_files[f], fcb
    fits_read, fcb, data, primary_header, exten_no=0
    overlap_angle = sxpar(primary_header, 'OVRLPANG')
    fits_close, fcb

    if (abs(overlap_angle - 45.0) gt overlap_angle_tolerance) then begin
      overlap_angle_warning = 1B
      mg_log, 'overlap angle %0.2f exceeds tolerance', overlap_angle, $
              name='comp', /warn
    endif
  endfor

  send_warning = overlap_angle_warning
  if (send_warning && notification_email ne '') then begin
    t = 1000.D * systime(/seconds)
    length = strtrim(fix(alog10(t)) + 1L > 15, 2)
    timestamp = string(t, format='(I0' + length + ')')
    tmp_filename = filepath(string(timestamp, format='(%"comp-%s.txt")'), /tmp)

    openw, lun, tmp_filename, /get_lun
    if (overlap_angle_warning) then printf, lun, 'overlap angle exceeds tolerance'

    printf, lun, ''
    log_filename = filepath(date_dir + '.log', root=log_dir)
    printf, lun, log_filename, format='(%"See warnings in log %s for details")'
    free_lun, lun

    subject = string(date_dir, wave_type, $
                     format='(%"Warnings for CoMP on %s (%s)")')
    mail_cmd = string(subject, notification_email, tmp_filename, $
                      format='(%"mail -s ''%s'' %s < %s")')
    spawn, mail_cmd, result, error_result, exit_status=status
    if (status ne 0L) then begin
      mg_log, 'problem with mail command: %s', mail_cmd, name='comp', /error
      mg_log, error_result, name='comp', /error
    endif

    file_delete, tmp_filename
  endif
end
