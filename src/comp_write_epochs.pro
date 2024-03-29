; docformat = 'rst'

;+
; Write the used values from the epochs.cfg to the engineering directory for
; the given date.
;
; :Params:
;   date_dir : in, required, type=string
;     date in the form 'YYYYMMDD'
;
; :Author:
;   MLSO Software Team
;-
pro comp_write_epochs, date_dir
  compile_opt strictarr
  @comp_constants_common
  @comp_mask_constants_common
  @comp_config_common

  filename = filepath(string(date_dir, format='(%"%s.comp.epochs.cfg")'), $
                      subdir=comp_decompose_date(date_dir), $
                      root=engineering_dir)
  openw, lun, filename, /get_lun

  printf, lun, 'debug', debug ? 'YES' : 'NO', format='(%"%-30s : %s")'

  printf, lun, 'nx', nx, format='(%"%-30s : %d")'
  printf, lun, 'ny', ny, format='(%"%-30s : %d")'

  printf, lun, 'wavefwhm', wavefwhm, format='(%"%-30s : %0.2f")'

  printf, lun

  printf, lun, 'center_1074', center1074, format='(%"%-30s : %0.2f")'
  printf, lun, 'center_1079', center1079, format='(%"%-30s : %0.2f")'
  printf, lun, 'center_1083', center1083, format='(%"%-30s : %0.2f")'

  printf, lun, '1074_3pt_wavelengths', wavelengths_3pt_1074, $
          format='(%"%-30s : [%0.2f, %0.2f, %0.2f]")'
  printf, lun, '1079_3pt_wavelengths', wavelengths_3pt_1079, $
          format='(%"%-30s : [%0.2f, %0.2f, %0.2f]")'
  printf, lun, '1083_3pt_wavelengths', wavelengths_3pt_1083, $
          format='(%"%-30s : [%0.2f, %0.2f, %0.2f]")'

  printf, lun, '1074_5pt_wavelengths', wavelengths_5pt_1074, $
          format='(%"%-30s : [%0.2f, %0.2f, %0.2f, %0.2f, %0.2f]")'
  printf, lun, '1079_5pt_wavelengths', wavelengths_5pt_1079, $
          format='(%"%-30s : [%0.2f, %0.2f, %0.2f, %0.2f, %0.2f]")'

  printf, lun

  printf, lun, 'display_min_1074', dispmin1074, format='(%"%-30s : %0.2f")'
  printf, lun, 'display_min_1079', dispmin1079, format='(%"%-30s : %0.2f")'
  printf, lun, 'display_min_1083', dispmin1083, format='(%"%-30s : %0.2f")'

  printf, lun, 'display_max_1074', dispmax1074, format='(%"%-30s : %0.2f")'
  printf, lun, 'display_max_1079', dispmax1079, format='(%"%-30s : %0.2f")'
  printf, lun, 'display_max_1083', dispmax1083, format='(%"%-30s : %0.2f")'

  printf, lun, 'display_exp_1074', dispexp1074, format='(%"%-30s : %0.2f")'
  printf, lun, 'display_exp_1079', dispexp1079, format='(%"%-30s : %0.2f")'
  printf, lun, 'display_exp_1083', dispexp1083, format='(%"%-30s : %0.2f")'

  printf, lun

  printf, lun, 'distortion_coeffs_file', distortion_coeffs_file, $
          format='(%"%-30s : %s")'

  printf, lun

  printf, lun, 'nominal_overlap_angle', nominal_overlap_angle, $
          format='(%"%-30s : %0.2f")'
  printf, lun, 'overlap_angle_tolerance', overlap_angle_tolerance, $
          format='(%"%-30s : %0.2f")'

  printf, lun, 'post_angle_diff_tolerance', post_angle_diff_tolerance, $
          format='(%"%-30s : %0.2f")'

  printf, lun, 'background_limit', background_limit, format='(%"%-30s : %0.2f")'
  printf, lun, 'min_flat_median_offset', min_flat_median_offset, $
          format='(%"%-30s : %0.2f")'


  printf, lun, 'check_lcvr6_temp', check_lcvr6_temp ? 'YES' : 'NO', $
          format='(%"%-30s : %s")'
  printf, lun, 'nominal_lcvr6_temp', nominal_lcvr6_temp, $
          format='(%"%-30s : %0.2f")'
  printf, lun, 'lcvr6_temp_tolerance', lcvr6_temp_tolerance, $
          format='(%"%-30s : %0.2f")'

  printf, lun, 'nominal_filt_temp', nominal_filt_temp, $
          format='(%"%-30s : %0.2f")'
  printf, lun, 'filt_temp_tolerance', filt_temp_tolerance, $
          format='(%"%-30s : %0.2f")'

  printf, lun, 'int_min_1074_thresh', int_min_1074_thresh, format='(%"%-30s : %0.2f")'
  printf, lun, 'int_min_1079_thresh', int_min_1079_thresh, format='(%"%-30s : %0.2f")'
  printf, lun, 'int_max_thresh', int_max_thresh, format='(%"%-30s : %0.2f")'
  printf, lun, 'diff_thresh', diff_thresh, format='(%"%-30s : %0.2f")'

  printf, lun

  printf, lun, 'stokes', stokes, format='(%"%-30s : [%s, %s, %s, %s]")'

  printf, lun

  printf, lun, 'post_rotation', post_rotation, format='(%"%-30s : %0.2f")'
  printf, lun, 'post_width', post_width, format='(%"%-30s : %0.2f")'
  printf, lun, 'occulter_offset', occulter_offset, format='(%"%-30s : %0.2f")'
  printf, lun, 'field_offset', field_offset, format='(%"%-30s : %0.2f")'
  printf, lun, 'field_overlap', field_overlap, format='(%"%-30s : %0.2f")'
  printf, lun, 'plate_scale', plate_scale, format='(%"%-30s : %0.2f")'

  printf, lun, 'post_angle_guess', post_angle_guess, format='(%"%-30s : %0.1f")'
  printf, lun, 'post_angle_tolerance', post_angle_tolerance, format='(%"%-30s : %0.1f")'

  printf, lun

  printf, lun, 'gbu_max_background', gbu_max_background, format='(%"%-30s : %0.1f")'
  printf, lun, 'gbu_min_background', gbu_min_background, format='(%"%-30s : %0.1f")'
  printf, lun, 'gbu_med_background', gbu_med_background, format='(%"%-30s : %0.1f")'
  printf, lun, 'gbu_max_sigma', gbu_max_sigma, format='(%"%-30s : %0.1f")'
  printf, lun, 'gbu_percent_background_change', gbu_percent_background_change, $
          format='(%"%-30s : %0.1f")'
  printf, lun, 'gbu_background_threshold', gbu_background_threshold, $
          format='(%"%-30s : %0.1f")'
  printf, lun, 'gbu_theshold_count', gbu_threshold_count, format='(%"%-30s : %d")'
  printf, lun, 'gbu_offset_count', gbu_offset_count, format='(%"%-30s : %d")'

  printf, lun

  printf, lun, 'i_to_q_xtalk', i_to_q_xtalk, format='(%"%-30s : %0.6f")'
  printf, lun, 'i_to_u_xtalk', i_to_u_xtalk, format='(%"%-30s : %0.6f")'

  printf, lun, 'u_to_q_xtalk', u_to_q_xtalk, format='(%"%-30s : %0.6f")'
  printf, lun, 'q_to_u_xtalk', q_to_u_xtalk, format='(%"%-30s : %0.6f")'

  printf, lun, 'i_to_v_xtalk', i_to_v_xtalk, format='(%"%-30s : %0.6f")'
  printf, lun, 'q_to_v_xtalk', q_to_v_xtalk, format='(%"%-30s : %0.6f")'
  printf, lun, 'u_to_v_xtalk', u_to_v_xtalk, format='(%"%-30s : %0.6f")'

  printf, lun

  printf, lun, 'nd1', transmissions[0], format='(%"%-30s : %0.8f")'
  printf, lun, 'nd2', transmissions[1], format='(%"%-30s : %0.8f")'
  printf, lun, 'nd3', transmissions[2], format='(%"%-30s : %0.8f")'
  printf, lun, 'nd4', transmissions[3], format='(%"%-30s : %0.8f")'
  printf, lun, 'nd5', transmissions[4], format='(%"%-30s : %0.8f")'
  printf, lun, 'nd6', transmissions[5], format='(%"%-30s : %0.8f")'
  printf, lun, 'nd7', transmissions[6], format='(%"%-30s : %0.8f")'
  printf, lun, 'nd8', transmissions[7], format='(%"%-30s : %0.8f")'

  free_lun, lun
end
