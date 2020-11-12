; docformat = 'rst'


;+
; Perform the checks on L1 files that are not wave type dependent.
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;
; :Author:
;   MLSO Software Team
;-
pro comp_l1_check_all, date_dir, body=body, no_log_message=no_log_message
  compile_opt strictarr
  @comp_config_common
  @comp_check_common
  @comp_constants_common

  body = list()

  body->add, '# All files'
  body->add, ''
  body->add, '## Basic statistics'
  body->add, ''

  for w = 0L, n_elements(process_wavelengths) - 1L do begin
    l1_files = comp_find_l1_file(date_dir, process_wavelengths[w], $
                                 /all, count=n_l1_files)
    body->add, string(n_l1_files, process_wavelengths[w], $
                      format='(%"%d %s nm files")')
  endfor

  body->add, ''

  body->add, '## Warnings'
  body->add, ''

  n_warnings = (n_images_off_detector gt 0L) + (n_flats_too_low gt 0L) + (n_bad_quality gt 0L)

  if (n_warnings eq 0L) then body->add, 'no warnings'

  if (n_images_off_detector gt 0L) then begin
    body->add, string(n_images_off_detector, format='(%"%d images off detector")')
    mg_log, '%d images off detector', n_images_off_detector, name='comp', /warn
  endif

  if (n_flats_too_low gt 0L) then begin
    body->add, string(n_flats_too_low, $
                      format='(%"%d flats rejected for being below threshold")')
    mg_log, '%d flats rejected for median below threshold', n_flats_too_low, $
            name='comp', /warn
  endif

  if (n_bad_quality gt 0L) then begin
    body->add, string(n_bad_quality, $
                      format='(%"%d science images failed quality check")')
    mg_log, '%d science images failed quality check', n_bad_quality, $
            name='comp', /warn
  endif

  log_filename = filepath(date_dir + '.comp.log', root=log_dir)

  if (not keyword_set(no_log_message)) then begin
    body->add, ''
    body->add, '## Log message warnings'
    body->add, ''
    warning_msgs = comp_filter_log(log_filename, /warning, n_messages=n_messages)
    if (n_messages eq 0L) then begin
      body->add, 'no warning messages'
    endif else begin
      body->add, warning_msgs, /extract
    endelse
  endif

  body->add, ''
  body->add, string(log_filename, format='(%"See log %s for details")')

  body->add, ['', ''], /extract
end
