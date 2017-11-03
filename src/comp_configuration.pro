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

  ; mission
  doi_1074 = config->get('1074_doi_url', section='mission', default='')
  doi_1079 = config->get('1079_doi_url', section='mission', default='')
  doi_1083 = config->get('1083_doi_url', section='mission', default='')

  ; save files
  binary_dir    = mg_src_root()
  hot_file      = filepath('hothot.sav', root=binary_dir)
  ffmpeg_dir    = config->get('ffmpeg_dir', section='externals')
  hsi_dir       = config->get('hsi_dir', section='externals', default='')

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

  ; notifications
  notification_email = config->get('email', section='notifications', default='')

  ; engineering files
  engineering_dir = config->get('engineering_dir', section='engineering')
  centering_diagnostics       = config->get('centering_diagnostics', $
                                            section='engineering', $
                                            /boolean, default=0B)
  flat_corrected_output       = config->get('flat_corrected_output', $
                                            section='engineering', $
                                            /boolean, default=0B)
  demodulated_output          = config->get('demodulated_output', $
                                            section='engineering', $
                                            /boolean, default=0B)

  ; actions
  dry_run             = config->get('dry_run', section='actions', $
                                    /boolean, default=0B)
  create_l1           = config->get('create_l1', section='actions', $
                                    /boolean, default=1B)
  perform_gbu         = config->get('perform_gbu', section='actions', $
                                    /boolean, default=create_l1)
  check_l1            = config->get('check_l1', section='actions', $
                                    /boolean, default=create_l1)
  create_flatsdarks   = config->get('create_flatsdarks', section='actions', $
                                    /boolean, default=create_l1)
  distribute_l1       = config->get('distribute_l1', section='actions', $
                                    /boolean, default=1B)
  create_l2           = config->get('create_l2', section='actions', $
                                    /boolean, default=1B)
  create_average      = config->get('create_average', section='actions', $
                                    /boolean, default=create_l2)
  create_quick_invert = config->get('create_quick_invert', section='actions', $
                                    /boolean, default=create_l2)
  create_full_invert = config->get('create_full_invert', section='actions', $
                                    /boolean, default=0B)
  find_systematics    = config->get('find_systematics', section='actions', $
                                    /boolean, default=create_l2)
  create_analysis     = config->get('create_analysis', section='actions', $
                                    /boolean, default=create_l2)
  create_daily_images = config->get('create_daily_images', section='actions', $
                                    /boolean, default=create_l2)
  create_movies       = config->get('create_movies', section='actions', $
                                    /boolean, default=create_l2)
  create_daily_summaries = config->get('create_daily_summaries', section='actions', $
                                       /boolean, default=create_l2)
  distribute_l2       = config->get('distribute_l2', section='actions', $
                                    /boolean, default=1B)
  mail_warnings       = config->get('mail_warnings', section='actions', $
                                    /boolean, default=1B)
  send_to_hpss        = config->get('send_to_hpss', section='actions', $
                                    /boolean, default=1B)
  validate            = config->get('validate', section='actions', $
                                    /boolean, default=1B)
  lock_raw            = config->get('lock_raw', section='actions', $
                                  /boolean, default=1B)

  ; database options
  update_database = config->get('update_database', section='database', $
                                /boolean, default=1B)
  database_config_filename = config->get('config_filename', $
                                         section='database', $
                                         default='')
  database_config_section = config->get('config_section', $
                                        section='database', $
                                        default='')

  ; processing code version
  code_version = comp_find_code_version(revision=revision, branch=branch)
  code_revision = revision
  code_branch = branch

  ; options
  subtract_background    = config->get('subtract_background', $
                                       section='options', $
                                       /boolean, default=1B)
  remove_stray_light     = config->get('remove_stray_light', $
                                       section='options', $
                                       /boolean, default=0B)
  empirical_crosstalk_calculation_mode $
      = config->get('empirical_crosstalk_calculation_mode', $
                    section='options', $
                    /boolean, default=0B)
  correct_crosstalk      = config->get('correct_crosstalk', $
                                       section='options', $
                                       /boolean, $
                                       default=~empirical_crosstalk_calculation_mode)
  perform_polarimetric_transform = config->get('perform_polarimetric_transform', $
                                               section='options', $
                                               /boolean, $
                                               default=~empirical_crosstalk_calculation_mode)


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

  ; L2 averaging
  averaging_max_n_files           = config->get('max_n_files', $
                                                section='averaging', $
                                                default=50, type=3)
  averaging_min_n_cluster_files    = config->get('min_n_cluster_files', $
                                                 section='averaging', $
                                                 default=40, type=3)
  averaging_min_n_qu_files         = config->get('min_n_qu_files', $
                                                 section='averaging', $
                                                 default=4, type=3)
  averaging_max_cadence_interval   = config->get('max_cadence_interval', $
                                                 section='averaging', $
                                                 /extract, type=4, $
                                                 default=[180.0, 360.0, 600.0])
  averaging_max_n_noncluster_files = config->get('max_n_noncluster_files', $
                                                 section='averaging', $
                                                 default=50, type=3)
  compute_mean                     = config->get('compute_mean', $
                                                 section='averaging', $
                                                 /boolean, default=1B)
  compute_median                   = config->get('compute_median', $
                                                 section='averaging', $
                                                 /boolean, default=1B)
  average_background_by_polarization = config->get('background_by_polarization', $
                                                   section='averaging', $
                                                   /boolean, $
                                                   default=empirical_crosstalk_calculation_mode)
end
