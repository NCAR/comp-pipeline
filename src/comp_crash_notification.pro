; docformat = 'rst'

;+
; Send notification when the pipeline crashes.
;
; :Params:
;   date : in, required, type=string
;     date running when crash occurred in the form 'YYYYMMDD'; may be undefined
;     if crash occurs before date determined
;-
pro comp_crash_notification, date
  compile_opt strictarr
  @comp_config_common

  ; add tag about pipeline and process at the end of body
  spawn, 'hostname', hostname, exit_status=status
  if (status ne 0) then hostname = ''

  help, /last_message, output=help_output
  body = [help_output, '']

  if (notification_email eq '') then begin
    mg_log, 'no email specified to send notification to', name=logger_name, /info
    goto, done
  endif else begin
    mg_log, 'sending crash notification to %s', notification_email, $
            name=logger_name, /info
  endelse

  log_filename = filepath(date + '.comp.log', root=log_dir)
  errors = comp_filter_log(log_filename, /error, n_messages=n_errors)
  body = [body, log_filename, '', errors]

  spawn, 'echo $(whoami)@$(hostname)', who, error_result, exit_status=status
  if (status eq 0L) then begin
    who = who[0]
  endif else begin
    who = 'unknown'
  endelse

  credit = string(mg_src_root(/filename), who, format='(%"Sent from %s (%s)")')

  subject = string(n_elements(date) eq 0L ? 'unknown date' : date, $
                   format='(%"CoMP crash during processing for %s")')
  body = [body, '', credit]

  comp_send_mail, notification_email, subject, body, error=error
  if (error ne 0L) then begin
    mg_log, 'problem sending crash notification', name='comp', /error
  endif

  done:
end
