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
pro comp_l1_check_all, date_dir, body=body
  compile_opt strictarr
  @comp_config_common
  @comp_check_common
  @comp_constants_common

  body = list()

  body->add, '# All files'
  body->add, ''
  body->add, '## Warnings'
  body->add, ''

  n_warnings = (n_images_off_detector gt 0L) + (n_flats_too_low)

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

  body->add, ''
  body->add, '## Log message warnings'
  body->add, ''
  log_filename = filepath(date_dir + '.comp.log', root=log_dir)
  warning_msgs = comp_filter_log(log_filename, /warning, n_messages=n_messages)
  if (n_messages eq 0L) then begin
    body->add, 'no warning messages'
  endif else begin
    body->add, warning_msgs, /extract
  endelse

  body->add, ''
  body->add, string(log_filename, format='(%"See log %s for details")')

  body->add, ['', ''], /extract
end
