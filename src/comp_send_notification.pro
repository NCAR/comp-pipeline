; docformat = 'rst'

;+
; Send the notification email
;
; :Params:
;   date : in, required, type=string
;     date of the form "YYYYMMDD"
;   body : in, required, type=object
;     list of strings for the body of the message
;   t0 : in, required, type=double
;     epoch time of the start of the pipeline for this date
;   gbu_plot_filename : in, optional, type=string
;     filename of GBU plot file
;-
pro comp_send_notification, date, body, t0, gbu_plot_filename
  compile_opt strictarr
  @comp_config_common

  ; add tag about pipeline and process at the end of body
  spawn, 'hostname', hostname, exit_status=status
  if (status ne 0) then hostname = ''
  
  body->add, string(mg_src_root(/filename), $
                    getenv('USER'), hostname, $
                    format='(%"Sent from %s (%s@%s)")')
  code_version = comp_find_code_version(revision=revision, branch=branch)
  body->add, string(code_version, revision, branch, $
                    format='(%"comp-pipeline %s (%s on %s)")')
  body->add, string(comp_sec2str(systime(/seconds) - t0), $
                    format='(%"Total runtime: %s")')

  ; send the email
  if (notification_email ne '') then begin
    mg_log, 'sending email to %s', notification_email, $
            name='comp', /info
    subject = string(date, format='(%"CoMP results for %s")')
    body_text = body->toArray()
    comp_send_mail, notification_email, subject, body_text, $
                    attachments=gbu_plot_filename
  endif else begin
    mg_log, 'not sending notification because no email', name='comp', /info
  endelse

  obj_destroy, body
end
