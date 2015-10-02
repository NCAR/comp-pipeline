; docformat = 'rst'

;+
; Set up paths and other settings for a run of the CoMP pipeline.
;
; :Uses:
;   comp_config_common, mg_read_config
;
; :Keywords:
;   config_filename, in, optional, type=string
;     configuration filename to use, default is `comp.cfg` in the `src`
;     directory
;
; :Author:
;   sitongia
;-
pro comp_configuration, config_filename=config_filename
  compile_opt strictarr
  @comp_config_common

  ; return if paths have already been defined
  if (n_elements(bias_dir) gt 0L) then return

  _config_filename = n_elements(config_filename) eq 0L $  
                       ? filepath('comp.cfg', root=mg_src_root()) $
                       : config_filename

  config = mg_read_config(_config_filename)

  ; calibration files
  bias_dir   = config->get('bias_dir', section='calibration')
  flat_dir   = config->get('flat_dir', section='calibration')
  mask_dir   = config->get('mask_dir', section='calibration')

  ; save files
  binary_dir    = config->get('binary_dir', section='save')
  hot_file      = config->get('hot_file', section='save')
  deferred_file = config->get('deferred_file', section='save')
  ffmpeg_dir    = config->get('ffmpeg_dir', section='save')
  logo_dir      = config->get('logo_dir', section='save')

  ; processing
  raw_basedir      = config->get('raw_basedir', section='processing')
  date_pattern     = config->get('date_pattern', section='processing', default='*')
  process_basedir  = config->get('process_basedir', section='processing')
  process_wavelengths = config->get('wavelengths', section='processing', /extract)

  ; flats
  flat_avg_skip_first = config->get('skip_first', section='flats', /boolean, default=0B)
  read_flats_beam_multiplies_wave = config->get('beam_multiplies_wave_on_read', section='flats', /boolean, default=1B)
  make_flat_beam_multiplies_wave = config->get('beam_multiplies_wave', section='flats', /boolean, default=1B)
  make_flat_detrending = config->get('detrending', section='flats', /boolean, default=0B)
  make_flat_destraying = config->get('destraying', section='flats', /boolean, default=0B)
  make_flat_fill = config->get('fill', section='flats', /boolean, default=1B)
  make_flat_spectral_correction = config->get('spectral_correction', section='flats', /boolean, default=0B)

  ; distribution of results
  archive_dir  = config->get('archive_dir', section='results')
  movie_dir    = config->get('movie_dir', section='results')
  fullres_dir  = config->get('fullres_dir', section='results')
  ldm_basedir  = config->get('ldm_basedir', section='results')
  hpss_gateway = config->get('hpss_gateway', section='results')

  ; log files
  log_dir = config->get('log_dir', section='log')
  log_level = long(config->get('level', section='log', default=4L))
  l1_process_log_level = long(config->get('l1_process_level', section='log', default=log_level))
  circfit_log_level = long(config->get('circfit_level', section='log', default=log_level))
  quick_invert_log_level = long(config->get('quick_invert_level', section='log', default=log_level))
  average_log_level = long(config->get('average_level', section='log', default=log_level))
  dark_interp_log_level = long(config->get('dark_interp_level', section='log', default=log_level))
  fix_crosstalk_log_level = long(config->get('fix_crosstalk_level', section='log', default=log_level))
  find_image_center_log_level = long(config->get('find_image_center', section='log', default=log_level))
  find_post_log_level = long(config->get('find_post', section='log', default=log_level))

  ; actions
  mail_warnings = fix(config->get('mail_warnings', section='actions', /boolean, default=1B))
  send_to_hpss = fix(config->get('send_to_hpss', section='actions', /boolean, default=1B))
  validate = fix(config->get('validate', section='actions', /boolean, default=1B))
end
