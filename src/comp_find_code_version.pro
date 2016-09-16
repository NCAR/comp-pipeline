; docformat = 'rst'

;+
; Determines the version of the code.
;
; :Returns:
;   string verion number, such as '1.0.0'
;
; :Keywords:
;   revision : out, optional, type=string
;     code repo revision value, such as '8207' or 'afc6d0'
;-
function comp_find_code_version, revision=revision
  compile_opt strictarr
  @comp_config_common

  cd, current=current_working_dir
  cd, binary_dir

  if (arg_present(revision)) then begin
    cmd = string(filepath('git', root=git_dir), $
                 format='(%"%s log -1 --pretty=format:%%h")')
    spawn, cmd, revision, error_result, exit_status=status
    if (status ne 0L) then begin
      mg_log, 'query for git repo status exited with status %d (%s)', $
              status, strjoin(error_result, ' '), $
              name='comp', /warning
      revision = '0'
    endif else revision = revision[0]
  endif

  cd, current_working_dir

  return, '1.2.3'
end
