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
  @comp_check_common

  l1_files = comp_find_l1_file(date_dir, wave_type, /all, count=n_l1_files)

  if (n_l1_files eq 0L) then begin
    mg_log, 'no L1 files to check', name='comp', /info
    return
  endif

  overlap_angle_warning = 0B
  background = fltarr(n_l1_files)

  ; TODO: other things to check for:
  ;   - annulus not inside CCD
  ;   - check temperatures

  for f = 0L, n_l1_files - 1L do begin
    mg_log, 'checking %s', file_basename(l1_files[f]), name='comp', /info

    ; check overlap angle deviation from 45 degrees
    fits_open, l1_files[f], fcb
    fits_read, fcb, data, primary_header, exten_no=0
    overlap_angle = sxpar(primary_header, 'OVRLPANG')
    background[f] = sxpar(primary_header, 'BACKGRND')
    fits_close, fcb

    if (abs(overlap_angle - 45.0) gt overlap_angle_tolerance) then begin
      overlap_angle_warning = 1B
      mg_log, 'overlap angle %0.2f exceeds tolerance', overlap_angle, $
              name='comp', /warn
    endif
  endfor

  if (n_images_off_detector gt 0L) then begin
    mg_log, '%d images off detector', n_files_off_detector, name='comp', /warn
  endif

  med_background = median(background)

  send_warning = overlap_angle_warning $
                   || med_background gt background_limit $
                   || n_images_off_detector gt 0L
  if (send_warning && notification_email ne '') then begin
    body = list()
    if (overlap_angle_warning) then body->add, 'overlap angle exceeds tolerance'
    if (med_background gt background_limit) then begin
      body->add, string(med_background, background_limit, $
                        format='(%"median background %0.1f exceeds limit %0.1f")')
    endif
    if (n_images_off_detector gt 0L) then begin
      body->add, string(n_images_off_detector, format='(%"%d images off detector")')
    endif

    body->add, ''
    log_filename = filepath(date_dir + '.log', root=log_dir)
    body->add, string(log_filename, format='(%"See warnings in log %s for details")')

    body_text = body->toArray()
    obj_destroy, body

    subject = string(date_dir, wave_type, $
                     format='(%"Warnings for CoMP on %s (%s nm)")')

    comp_send_mail, notification_email, subject, body_text
  endif
end
