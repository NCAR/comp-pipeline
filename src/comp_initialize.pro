; docformat = 'rst'


;+
; Lookup a value for a single parameter based on the date in the `epochs.cfg`
; configuration file.
;
; :Returns:
;   by default returns a string, unless `TYPE` is specified
;
; :Params:
;   option : in, required, type=string
;     option name
;   date : in, required, type=string
;     date on which to check for the value in the form "YYYYMMDD"
;   options : in, required, type=MGffOptions
;     options to check for value in
;
; :Keywords:
;   found : out, optional, type=boolean
;     set to a named variable to retrieve whether the option was found
;   type : in, optional, type=integer
;     `SIZE` type to retrieve value as
;   _extra : in, optional, type=keywords
;     keywords to `MGffOptions::get` such as `BOOLEAN` and `EXTRACT`
;-
function comp_initialize_readconfig, option, date, options, $
                                     found=found, $
                                     type=type, $
                                     _extra=e
  compile_opt strictarr

  found = 1B
  dates = options->sections()
  dates = dates[sort(dates)]
  date_index = value_locate(dates, date)
  for d = date_index, 0L, -1L do begin
    option_value = options->get(option, section=dates[d], $
                                found=option_found, type=type, _extra=e)
    if (option_found) then begin
      return, option_value
    endif
  endfor

  found = 0B
  return, !null
end


;+
; Initialize constants for CoMP pipeline for a running on data from a specific
; date.
;
; Julian Date is used for selections. It can be computed from the data using::
;
;   jd = julday(month, day, year, hour, minute, second)
;
; :Uses:
;   comp_constants_common, comp_mask_constants_common
;
; :Params:
;    date_dir : in, required, type=string
;      date to process, in YYYYMMDD format
;
; :Author:
;   sitongia
;-
pro comp_initialize, date_dir
  compile_opt strictarr
  @comp_constants_common
  @comp_mask_constants_common
  @comp_check_common

  options = mg_read_config(filepath('epochs.cfg', root=mg_src_root()))

  debug = comp_initialize_readconfig('debug', date_dir, options, /boolean)
  overlap_angle_tolerance = comp_initialize_readconfig('overlap_angle_tolerance', $
                                                       date_dir, options, type=4)
  background_limit = comp_initialize_readconfig('background_limit', $
                                                date_dir, options, type=4)

  ; level 1 image dimensions
  nx = comp_initialize_readconfig('nx', date_dir, options, type=3)
  ny = comp_initialize_readconfig('ny', date_dir, options, type=3)


  ; thresholds for cutting out bad data in the L2 products

  ; millionths of solar disk intensity
  int_thresh  = comp_initialize_readconfig('int_thresh', date_dir, options, type=4)
  ; difference between measured and calculated line center intensity
  diff_thresh = comp_initialize_readconfig('diff_thresh', date_dir, options, type=4)

  ; line center wavelengths
  center1074 = comp_initialize_readconfig('center_1074', date_dir, options, type=4)
  center1079 = comp_initialize_readconfig('center_1079', date_dir, options, type=4)
  center1083 = comp_initialize_readconfig('center_1083', date_dir, options, type=4)

  wavelengths_3pt_1074 = comp_initialize_readconfig('1074_3pt_wavelengths', $
                                                    date_dir, options, $
                                                    type=4, /extract)
  wavelengths_3pt_1079 = comp_initialize_readconfig('1079_3pt_wavelengths', $
                                                    date_dir, options, $
                                                    type=4, /extract)
  wavelengths_3pt_1083 = comp_initialize_readconfig('1083_3pt_wavelengths', $
                                                    date_dir, options, $
                                                    type=4, /extract)

  wavelengths_5pt_1074 = comp_initialize_readconfig('1074_5pt_wavelengths', $
                                                    date_dir, options, $
                                                    type=4, /extract)
  wavelengths_5pt_1079 = comp_initialize_readconfig('1079_5pt_wavelengths', $
                                                    date_dir, options, $
                                                    type=4, /extract)

  ; display values
  dispmin1074 = comp_initialize_readconfig('display_min_1074', date_dir, options, type=4)
  dispmin1079 = comp_initialize_readconfig('display_min_1079', date_dir, options, type=4)
  dispmin1083 = comp_initialize_readconfig('display_min_1083', date_dir, options, type=4)

  dispmax1074 = comp_initialize_readconfig('display_max_1074', date_dir, options, type=4)
  dispmax1079 = comp_initialize_readconfig('display_max_1079', date_dir, options, type=4)
  dispmax1083 = comp_initialize_readconfig('display_max_1083', date_dir, options, type=4)

  dispexp1074 = comp_initialize_readconfig('display_exp_1074', date_dir, options, type=4)
  dispexp1079 = comp_initialize_readconfig('display_exp_1079', date_dir, options, type=4)
  dispexp1083 = comp_initialize_readconfig('display_exp_1083', date_dir, options, type=4)

  ; number of stokes parameters
  stokes = comp_initialize_readconfig('stokes', date_dir, options, /extract)
  n_stokes = n_elements(stokes)

  ; distortion coefficients
  distortion_coeffs_file = comp_initialize_readconfig('distortion_coeffs_file', $
                                                      date_dir, options)

  wavefwhm = comp_initialize_readconfig('wavefwhm', date_dir, options, type=4)

  ; parse the date_dir to find the Julian date to use to switch era of constants
  year  = fix(strmid(date_dir, 0, 4))
  month = fix(strmid(date_dir, 4, 2))
  day   = fix(strmid(date_dir, 6, 2))
  jd    = julday(month, day, year, 0, 0, 0)

  ; Era-specific correction factors

  ; offset of occulter post (pixels), positive shifts post clockwise in mask
  post_rotation = comp_initialize_readconfig('post_rotation', date_dir, options, type=4)

  ; width of the post (pixels)
  post_width = comp_initialize_readconfig('post_width', date_dir, options, type=4)

  ; over or undersize occulter mask
  occulter_offset = comp_initialize_readconfig('occulter_offset', date_dir, options, type=4)

  ; over or undersize field mask
  field_offset = comp_initialize_readconfig('field_offset', date_dir, options, type=4)

  ; overlap of two beams, creating "ears"
  field_overlap = comp_initialize_readconfig('field_overlap', date_dir, options, type=4)

  ; arcsec per pixel
  plate_scale = comp_initialize_readconfig('plate_scale', date_dir, options, type=4)

  ; crosstalk coefficients
  i_to_q_xtalk = comp_initialize_readconfig('i_to_q_xtalk', date_dir, options, type=4)
  i_to_u_xtalk = comp_initialize_readconfig('i_to_u_xtalk', date_dir, options, type=4)
  u_to_q_xtalk = comp_initialize_readconfig('u_to_q_xtalk', date_dir, options, type=4)
  q_to_u_xtalk = comp_initialize_readconfig('q_to_u_xtalk', date_dir, options, type=4)
  i_to_v_xtalk = comp_initialize_readconfig('i_to_v_xtalk', date_dir, options, type=4)
  q_to_v_xtalk = comp_initialize_readconfig('q_to_v_xtalk', date_dir, options, type=4)
  u_to_v_xtalk = comp_initialize_readconfig('u_to_v_xtalk', date_dir, options, type=4)

  transmissions = [comp_initialize_readconfig('nd1', date_dir, options, type=4), $
                   comp_initialize_readconfig('nd2', date_dir, options, type=4), $
                   comp_initialize_readconfig('nd3', date_dir, options, type=4), $
                   comp_initialize_readconfig('nd4', date_dir, options, type=4), $
                   comp_initialize_readconfig('nd5', date_dir, options, type=4), $
                   comp_initialize_readconfig('nd6', date_dir, options, type=4), $
                   comp_initialize_readconfig('nd7', date_dir, options, type=4), $
                   comp_initialize_readconfig('nd8', date_dir, options, type=4)]

  obj_destroy, options

  n_images_off_detector = 0L
end
