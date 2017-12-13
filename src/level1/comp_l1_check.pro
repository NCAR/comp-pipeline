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

    ; check overlap angle deviation from its nominal value
    fits_open, l1_files[f], fcb
    fits_read, fcb, data, primary_header, exten_no=0
    overlap_angle = sxpar(primary_header, 'OVRLPANG')

    im_background = sxpar(primary_header, 'BACKGRND')
    background[f] = size(im_background, /type) eq 7 ? !values.f_nan : im_background

    if (abs(overlap_angle - nominal_overlap_angle) gt overlap_angle_tolerance) then begin
      overlap_angle_warning = 1B
      mg_log, 'overlap angle %0.1f outside normal range %0.1f-%0.1f', $
              overlap_angle, $
              nominal_overlap_angle - overlap_angle_tolerance, $
              nominal_overlap_angle + overlap_angle_tolerance, $
              name='comp', /warn
    endif
 
    n_images_bad_temp_file = 0L
    n_images_bad_filttemp_file = 0L
    for e = 1L, fcb.nextend do begin
      fits_read, fcb, date, header, exten_no=e

      lcvr6temp = sxpar(header, 'LCVR6TMP')
      min_lcvr6temp = nominal_lcvr6_temp - lcvr6_temp_tolerance
      max_lcvr6temp = nominal_lcvr6_temp + lcvr6_temp_tolerance
      if (check_lcvr6_temp && (lcvr6temp lt min_lcvr6temp || lcvr6temp gt max_lcvr6temp)) then begin
        n_images_bad_temp += 1
        n_images_bad_temp_file += 1
      endif

      filttemp = sxpar(header, 'FILTTEMP')
      min_filttemp = nominal_filt_temp - filt_temp_tolerance
      max_filttemp = nominal_filt_temp + filt_temp_tolerance
      if (filttemp lt min_filttemp || filttemp gt max_filttemp) then begin
        n_images_bad_filttemp += 1
        n_images_bad_filttemp_file += 1
      endif
    endfor

    if (n_images_bad_temp_file gt 0L) then begin
      mg_log, 'LCVR6 temp outside of normal range %0.1f-%0.1f for %d images', $
              min_lcvr6temp, max_lcvr6temp, n_images_bad_temp_file, $
              name='comp', /warn
    endif

    if (n_images_bad_filttemp_file gt 0L) then begin
      mg_log, 'filter temp outside of normal range %0.1f-%0.1f for %d images', $
              min_filttemp, max_filttemp, n_images_bad_filttemp_file, $
              name='comp', /warn
    endif

    fits_close, fcb
  endfor

  if (n_images_off_detector gt 0L) then begin
    mg_log, '%d images off detector', n_images_off_detector, name='comp', /warn
  endif

  med_background = median(background)

  eng_dir = filepath('', subdir=comp_decompose_date(date_dir), root=engineering_dir)
  if (~file_test(eng_dir, /directory)) then file_mkdir, eng_dir
  med_back_basename = string(date_dir, wave_type, $
                             format='(%"%s.comp.%s.background.txt")')
  med_back_filename = filepath(med_back_basename, root=eng_dir)
  openw, lun, med_back_filename, /get_lun
  printf, lun, med_back, format='(%"%0.1f")'
  free_lun, lun

  case wave_type of
    '1074': n_files_post_angle_diff = n_1074_files_post_angle_diff
    '1079': n_files_post_angle_diff = n_1079_files_post_angle_diff
    '1083': n_files_post_angle_diff = n_1083_files_post_angle_diff
  endcase

  reasons = ['data doesn''t exist on disk, but is in inventory file', $
             'standard 3 wavelengths not found for non-1083 data', $
             string(gbu_max_background, format='(%"background > max %0.1f ppm")'), $
             string(gbu_min_background, format='(%"background < min %0.1f ppm")'), $
             string(gbu_max_sigma, format='(%"std dev of intensity - median intensity > %0.2f ppm")'), $
             string(gbu_percent_background_change * 100.0, $
                    format='(%"background changes by more than %0.1f%% of median background")'), $
             string(gbu_threshold_count, gbu_background_threshold, $
                    format='(%"background contains more than %d pixels with value > %0.1f")'), $
             'std dev of intensity - median intensity is NaN of Inf']

  n_reasons = n_elements(reasons)
  bad_for_reason = lonarr(n_reasons)
  gbu_basename = string(date_dir, wave_type, format='(%"%s.comp.%s.gbu.log")')
  gbu_filename = filepath(gbu_basename, $
                          subdir=[date_dir, 'level1'], $
                          root=process_basedir)
  gbu = comp_read_gbu(gbu_filename)

  reason = 1L
  for r = 0L, n_reasons - 1L do begin
    !null = where((gbu.reason and reason) eq reason, n_bad_files)
    bad_for_reason[r] = n_bad_files
    reason = ishft(reason, 1)
  endfor

  ind = where(bad_for_reason, n_bad_reasons)

  n_warnings = overlap_angle_warning $
                 + (med_background gt background_limit) $
                 + (n_files_post_angle_diff gt 0L) $
                 + (n_images_off_detector gt 0L) $
                 + (n_images_bad_temp gt 0L) $
                 + (n_images_bad_filttemp gt 0L)

  send_warning = (n_warnings gt 0L) || (n_bad_reasons gt 0L)
  if (send_warning && notification_email ne '') then begin
    mg_log, 'sending warnings to %s', notification_email, name='comp', /info

    body = list()

    body->add, '# Warnings'

    body->add, ''

    if (n_warnings eq 0L) then body->add, 'no warnings'

    if (overlap_angle_warning) then body->add, 'overlap angle exceeds tolerance'
    if (med_background gt background_limit) then begin
      body->add, string(med_background, background_limit, $
                        format='(%"median background %0.1f exceeds limit %0.1f")')
    endif

    if (n_files_post_angle_diff) then begin
      body->add, string(n_files_post_angle_diff, $
                        format='(%"%d files with post angle difference greater than tolerance")')
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
    body->add, string(log_filename, format='(%"See log %s for details")')

    body->add, ['', '', '# GBU'], /extract

    body->add, ''
    if (n_bad_reasons eq 0L) then body->add, 'no bad files in GBU'

    for r = 0L, n_bad_reasons - 1L do begin
      body->add, string(bad_for_reason[ind[r]], reasons[ind[r]], $
                        format='(%"%d bad images because %s")')
    endfor

    body->add, ['', ''], /extract
    body->add, string(mg_src_root(/filename), $
                      getenv('USER'), getenv('HOSTNAME'), $
                      format='(%"Sent from %s (%s@%s)")')
    code_version = comp_find_code_version(revision=revision, branch=branch)
    body->add, string(code_version, revision, branch, $
                      format='(%"comp-pipeline %s (%s on %s)")')

    body_text = body->toArray()
    obj_destroy, body

    subject = string(date_dir, wave_type, $
                     format='(%"Warnings for CoMP on %s (%s nm)")')

    comp_send_mail, notification_email, subject, body_text
  endif
end


; main-level example program

date = '20171001'
config_basename = 'comp.mgalloy.mahi.latest.cfg'
config_filename = filepath(config_basename, $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())

comp_configuration, config_filename=config_filename
comp_initialize, date

comp_l1_check, date, '1074'

end
