; docformat = 'rst'

;+
; Validate files transferred from MLSO.
;
; :Uses:
;   comp_constants_common, comp_config_common, comp_initialize,
;   comp_configuration
;
; :Returns:
;   1 if valid, 0 if not valid
;
; :Examples:
;   For example, call like::
;
;     comp_validator, '20131015'
;
; :Params:
;   date_dir : in, required, type=string
;     day of year to process, in `YYYYMMDD` format
;
; :Author:
;   Berkey, modified by Sitongia and de Toma
;
; :History:
;   Fixed logical error: code was creating a tar file and sending it to the 
;                        mass-store even if validation test had failed (GdT)
;   Added tarlist for tar file (GdT)
;   Replaced spawn commands with unix script zipCoMPL0.sh that runs in the bkg 
;   so processing code can start without waiting for the tar file to be created 
;   tarring is done with 12h delay so it does not use CPU during processing
;   (GdT)
;   Made a more generic tar_and_hpss.sh script to be used for L1 and L2 results
;   as well. (mdg)
;-
function comp_validator, date_dir
  compile_opt idl2
  @comp_constants_common
  @comp_config_common

  ; configure
  comp_initialize, date_dir
  comp_configuration

  mg_log, 'starting', name='comp', /info

  raw_dir = filepath(date_dir, root=raw_basedir)
  cd, raw_dir

  invalid = 0

  done_file = date_dir + '.comp.t1.log'
  if (~file_test(done_file)) then begin
    mg_log, 'no t1.log file', name='comp', /warn
    return, 0
  endif

  numFiles = file_lines(done_file)

  if (numFiles gt 0) then begin
    tempStr = ''
    openr, lun, done_file, /get_lun
    while (~eof(lun)) do begin
      readf, lun, tempStr
      fileData = strsplit(tempStr, /extract)
      if (file_test(fileData[0])) then begin
        filesize = (file_info(fileData[0])).size
        if (fix(fileSize) ne fix(fileData[1])) then begin
          invalid = 1
          mg_log, '%s has a bad filesize', fileData[0], name='comp', /warn
        endif
      endif else begin
        invalid = 1
        mg_log, '%s was not found', fileData[0], name='comp', /warn
      endelse
    endwhile
    free_lun, lun
  endif else begin
    invalid = 1
    mg_log, 'done file is empty', name='comp', /warn
  endelse

  if (invalid) then begin
    mg_log, 'errors in validating files transferred from MLSO for %s', $
            date_dir, name='comp', /error
    if (mail_warnings) then begin
      mail_cmd = "mail -s 'Errors in validating files transferred from MLSO for CoMP on " $
                   + date_dir + "' ldm@hao.ucar.edu < /dev/null"
      spawn, mail_cmd, result, error_result, exit_status=status
      if (status ne 0L) then begin
        mg_log, 'problem with mail command: %s', mail_cmd, name='comp', /error
        mg_log, error_result, name='comp', /error
      endif
    endif
    return, ~invalid
  endif else begin
    ; added else so it zips and stores files only if validation worked (GdT)
    ; added list of tar file (GdT)

    ; replaced spawn commands with script
    if (send_to_hpss) then begin
      mg_log, 'tarring and sending L0 to HPSS', name='comp', /info
      if (~file_test(hpss_gateway, /directory)) then file_mkdir, hpss_gateway

      time_delay = '12h'
      archive_script = filepath('archive_l0.sh', $
                                subdir=['..', 'scripts'], $
                                root=binary_dir)
      cmd = string(archive_script, date_dir, hpss_gateway, time_delay, $
                   format='(%"%s %s %s %s &")')
      
      spawn, cmd, result, error_result, exit_status=status
      if (status ne 0L) then begin
        mg_log, 'problem sending data to HPSS with command: %s', cmd, $
                name='comp', /error
        mg_log, '%s', error_result, $
                name='comp', /error
      endif
    endif
  endelse

  mg_log, 'done', name='comp', /info
  return, ~invalid
end
