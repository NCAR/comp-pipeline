; docformat = 'rst'

;+
; Create engineering plots for the day.
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength to process, '1074', '1079', etc.
;
; :Author:
;   MLSO Software Team
;-
pro comp_plot_engineering, date_dir, wave_type
  compile_opt strictarr
  @comp_config_common

  eng_dir = filepath('', subdir=comp_decompose_date(date_dir), root=engineering_dir)

  mg_log, 'plotting daily centering values', name='comp', /info
  comp_plot_centering, eng_dir, $
                       filepath(string(date_dir, wave_type, $
                                       format='(%"%s.comp.%s.centering.ps")'), $
                                root=eng_dir), $
                       date_dir, $
                       wave_type
end
