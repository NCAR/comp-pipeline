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

  ; spawn, 'svnversion /home/sitongia/projects/CoMP/Pipeline', revision, $
  ;        errorResult, exit_status=status
  ; if (status ne 0L) then begin
  ;   mg_log, 'svnversion exited with status %d (%s)', status, errorResult, $
  ;           /warning, name='comp'
  ;   revision = '0'
  ; endif else revision = revision[0]

  mg_log, 'code revision currently not supported', name='comp', /warn

  revision = ''
  return, ''
end