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
;
; :Author:
;   MLSO Software Team
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

  n_images_bad_temp = 0L
  n_images_bad_filttemp = 0L
  overlap_angle_warning = 0B
  background = fltarr(n_l1_files)

  for f = 0L, n_l1_files - 1L do begin
    mg_log, 'checking %s', file_basename(l1_files[f]), name='comp', /info

    ; check overlap angle deviation from 45 degrees
    fits_open, l1_files[f], fcb
    fits_read, fcb, data, primary_header, exten_no=0
    overlap_angle = sxpar(primary_header, 'OVRLPANG')

    im_background = sxpar(primary_header, 'BACKGRND')
    background[f] = size(im_background, /type) eq 7 ? !values.f_nan : im_background

    if (abs(overlap_angle - 45.0) gt overlap_angle_tolerance) then begin
      overlap_angle_warning = 1B
      mg_log, 'overlap angle %0.1f outside normal range %0.1f-%0.1f', $
              overlap_angle, $
              45.0 - overlap_angle_tolerance, $
              45.0 + overlap_angle_tolerance, $
              name='comp', /warn
    endif
    
    for e = 1L, fcb.nextend do begin
      fits_read, fcb, date, header, exten_no=e

      lcvr6temp = sxpar(header, 'LCVR6TMP')
      min_lcvr6temp = 25.0
      max_lcvr6temp = 35.0
      if (lcvr6temp lt min_lcvr6temp || lcvr6temp gt max_lcvr6temp) then begin
        n_images_bad_temp += 1
        mg_log, 'LCVR6 temp %0.1f outside of normal range %0.1f-%0.1f', $
                lcvr6temp, min_lcvr6temp, max_lcvr6temp, $
                name='comp', /warn
      endif

      filttemp = sxpar(header, 'FILTTEMP')
      min_filttemp = 25.0
      max_filttemp = 35.0
      if (filttemp lt min_filttemp || filttemp gt max_filttemp) then begin
        n_images_bad_filttemp += 1
        mg_log, 'filter temp %0.1f outside of normal range %0.1f-%0.1f', $
                filttemp, min_filttemp, max_filttemp, $
                name='comp', /warn
      endif
    endfor

    fits_close, fcb
  endfor

  if (n_images_off_detector gt 0L) then begin
    mg_log, '%d images off detector', n_images_off_detector, name='comp', /warn
  endif

  med_background = median(background)

  send_warning = overlap_angle_warning $
                   || (med_background gt background_limit) $
                   || (n_images_off_detector gt 0L) $
                   || (n_images_bad_temp gt 0L) $
                   || (n_images_bad_filttemp gt 0L)
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
    if (n_images_bad_temp gt 0L) then begin
      body->add, string(n_images_bad_temp, $
                        format='(%"%d images with bad temperature (LCVR6TMP)")')
    endif
    if (n_images_bad_filttemp gt 0L) then begin
      body->add, string(n_images_bad_filttemp, $
                        format='(%"%d images with bad temperature (FILTTEMP)")')
    endif

    body->add, ''
    log_filename = filepath(date_dir + '.log', root=log_dir)
    body->add, string(log_filename, format='(%"See warnings in log %s for details")')
    body->add, ['', ''], /extract
    body->add, string(mg_src_root(/filename), $
                      getenv('USER'), getenv('HOSTNAME'), $
                      format='(%"Sent from %s (%s@%s")')

    body_text = body->toArray()
    obj_destroy, body

    subject = string(date_dir, wave_type, $
                     format='(%"Warnings for CoMP on %s (%s nm)")')

    comp_send_mail, notification_email, subject, body_text
  endif
end
