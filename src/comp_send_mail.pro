; docformat = 'rst'


;+
; Send an email.
;
; :Params:
;   address : in, required, type=string
;     email address to send to
;   subject : in, required, type=string
;     subject of email
;   body : in, optional, type=strarr
;     text of body of email
;
; :Keywords:
;   error : out, optional, type=long
;     error status, 0 if no error
;   attachments : in, optional, type=strarr
;     filenames of attachments
;
; :Author:
;   MLSO Software Team
;-
pro comp_send_mail, address, subject, body, $
                    attachments=attachments, $
                    error=error
  compile_opt strictarr

  if (n_elements(body) eq 0L) then begin
    body_filename = '/dev/null'
  endif else begin
    t = 1000.D * systime(/seconds)
    length = strtrim(fix(alog10(t)) + 1L > 15, 2)
    timestamp = string(t, format='(I0' + length + ')')
    body_filename = filepath(string(timestamp, format='(%"comp-%s.txt")'), /tmp)

    openw, lun, body_filename, /get_lun
    printf, lun, transpose(body)
    free_lun, lun
  endelse

  _attachments = n_elements(attachments) eq 0L $
                   ? '' $
                   : (strjoin('-a ' + attachments, ' '))

  cmd = string(subject, _attachments, address, body_filename, $
               format='(%"mail -s ''%s'' %s -r $(whoami)@ucar.edu %s < %s")')
  spawn, cmd, result, error_result, exit_status=error
  if (error ne 0L) then begin
    mg_log, 'problem with mail command: %s', cmd, name='comp', /error
    mg_log, error_result, name='comp', /error
  endif

  if (n_elements(body) gt 0L) then file_delete, body_filename
end
