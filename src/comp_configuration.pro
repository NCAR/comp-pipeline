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
  if (n_elements(raw_basedir) gt 0L) then return

  _config_filename = n_elements(config_filename) eq 0L $  
                       ? filepath('comp.cfg', root=mg_src_root()) $
                       : config_filename

  config = mg_read_config(_config_filename)

  ; save files
  binary_dir    = config->get('binary_dir', section='save')
  hot_file      = config->get('hot_file', section='save')
  ffmpeg_dir    = config->get('ffmpeg_dir', section='save')
  git_dir       = config->get('git_dir', section='save')

  ; processing
  raw_basedir         = config->get('raw_basedir', section='processing')
  date_pattern        = config->get('date_pattern', section='processing', $
                                    default='*')
  process_basedir     = config->get('process_basedir', section='processing')
  process_wavelengths = config->get('wavelengths', section='processing', $
                                    /extract)

  ; distribution of results
  archive_dir  = config->get('archive_dir', section='results')
  movie_dir    = config->get('movie_dir', section='results')
  fullres_dir  = config->get('fullres_dir', section='results')
  hpss_gateway = config->get('hpss_gateway', section='results')

  ; log files
  log_dir                     = config->get('log_dir', section='log')
  log_level                   = config->get('level', section='log', $
                                            default=4L, type=3)
  l1_process_log_level        = config->get('l1_process_level', section='log', $
                                            default=log_level, type=3)
  circfit_log_level           = config->get('circfit_level', section='log', $
                                            default=log_level, type=3)
  quick_invert_log_level      = config->get('quick_invert_level', section='log', $
                                            default=log_level, type=3)
  average_log_level           = config->get('average_level', section='log', $
                                            default=log_level, type=3)
  dark_interp_log_level       = config->get('dark_interp_level', section='log', $
                                            default=log_level, type=3)
  fix_crosstalk_log_level     = config->get('fix_crosstalk_level', section='log', $
                                            default=log_level, type=3)
  find_image_center_log_level = config->get('find_image_center', section='log', $
                                            default=log_level, type=3)
  find_post_log_level         = config->get('find_post', section='log', $
                                            default=log_level, type=3)

  ; engineering files
  engineering_dir = config->get('engineering_dir', section='engineering')

  ; actions
  create_l1         = config->get('create_l1', section='actions', $
                                  /boolean, default=1B)
  correct_crosstalk = config->get('correct_crosstalk', section='actions', $
                                  /boolean, default=1B)
  create_flatsdarks = config->get('create_flatsdarks', section='actions', $
                                  /boolean, default=create_l1)
  distribute_l1     = config->get('distribute_l1', section='actions', $
                                  /boolean, default=1B)
  create_l2         = config->get('create_l2', section='actions', $
                                  /boolean, default=1B)
  distribute_l2     = config->get('distribute_l2', section='actions', $
                                  /boolean, default=1B)
  mail_warnings     = config->get('mail_warnings', section='actions', $
                                  /boolean, default=1B)
  send_to_hpss      = config->get('send_to_hpss', section='actions', $
                                  /boolean, default=1B)
  validate          = config->get('validate', section='actions', $
                                  /boolean, default=1B)
  lock_raw          = config->get('lock_raw', section='actions', $
                                  /boolean, default=1B)
  update_database = config->get('update_database', section='actions', $
                                /boolean, default=1B)

  ; processing code version
  code_version = comp_find_code_version(revision=revision)
  code_revision = revision

  ; flats
  flat_avg_skip_first             = config->get('skip_first', section='flats', $
                                                /boolean, default=0B)
  read_flats_beam_multiplies_wave = config->get('beam_multiplies_wave_on_read', $
                                                section='flats', $
                                                /boolean, default=1B)
  make_flat_beam_multiplies_wave  = config->get('beam_multiplies_wave', $
                                                section='flats', $
                                                /boolean, default=1B)
  make_flat_detrending            = config->get('detrending', section='flats', $
                                                /boolean, default=0B)
  make_flat_destraying            = config->get('destraying', section='flats', $
                                                /boolean, default=0B)
  make_flat_fill                  = config->get('fill', section='flats', $
                                                /boolean, default=1B)
  make_flat_spectral_correction   = config->get('spectral_correction', $
                                                section='flats', $
                                                /boolean, default=0B)
  cache_flats                     = config->get('cache_flats', $
                                                section='flats', $
                                                /boolean, default=1B)
end
