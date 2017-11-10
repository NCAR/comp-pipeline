; docformat = 'rst'

;+
; :Author:
;   MLSO Software Team
;-
pro comp_run_unittests
  compile_opt strictarr

  mgunit, 'comp_uts'
end
