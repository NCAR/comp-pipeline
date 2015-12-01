; docformat = 'rst'


;+
; Lookup a value for a single parameter based on the date in the `epochs.cfg`
; configuration file.
;
; :Returns:
;   by default returns a string, unless `FLOAT`, `INTEGER`, or `BOOLEAN` is set
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
;   float : in, optional, type=boolean
;     set to convert value to a float
;   integer : in, optional, type=integer
;     set to convert value to a integer
;   _extra : in, optional, type=keywords
;     keywords to `MGffOptions::get` such as `BOOLEAN` and `EXTRACT`
;-
function comp_initialize_readconfig, option, date, options, $
                                     found=found, $
                                     float=float, integer=integer, $
                                     _extra=e
  compile_opt strictarr

  found = 1B
  dates = options->sections()
  dates = dates[sort(dates)]
  date_index = value_locate(dates, date)
  for d = date_index, 0L, -1L do begin
    option_value = options->get(option, section=dates[d], $
                                found=option_found, _extra=e)
    if (option_found) then begin
      case 1 of
        keyword_set(float): return, float(option_value)
        keyword_set(integer): return, long(option_value)
        else: return, option_value
      endcase
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

  options = mg_read_config(filepath('epochs.cfg', root=mg_src_root()))

  debug = comp_initialize_readconfig('debug', date_dir, options, /boolean)

  ; level 1 image dimensions
  nx = comp_initialize_readconfig('nx', date_dir, options, /integer)
  ny = comp_initialize_readconfig('ny', date_dir, options, /integer)


  ; thresholds for cutting out bad data in the L2 products

  ; millionths of solar disk intensity
  int_thresh  = comp_initialize_readconfig('int_thresh', date_dir, options, /integer)
  ; difference between measured and calculated line center intensity
  diff_thresh = comp_initialize_readconfig('diff_thresh', date_dir, options, /integer)

  ; line center wavelengths
  center1074 = comp_initialize_readconfig('center_1074', date_dir, options, /float)
  center1079 = comp_initialize_readconfig('center_1079', date_dir, options, /float)
  center1083 = comp_initialize_readconfig('center_1083', date_dir, options, /float)

  ; display values
  dispmin1074 = comp_initialize_readconfig('display_min_1074', date_dir, options, /float)
  dispmin1079 = comp_initialize_readconfig('display_min_1079', date_dir, options, /float)
  dispmin1083 = comp_initialize_readconfig('display_min_1083', date_dir, options, /float)

  dispmax1074 = comp_initialize_readconfig('display_max_1074', date_dir, options, /float)
  dispmax1079 = comp_initialize_readconfig('display_max_1079', date_dir, options, /float)
  dispmax1083 = comp_initialize_readconfig('display_max_1083', date_dir, options, /float)

  dispexp1074 = comp_initialize_readconfig('display_exp_1074', date_dir, options, /float)
  dispexp1079 = comp_initialize_readconfig('display_exp_1079', date_dir, options, /float)
  dispexp1083 = comp_initialize_readconfig('display_exp_1083', date_dir, options, /float)

  ; number of stokes parameters
  stokes = comp_initialize_readconfig('stokes', date_dir, options, /extract)
  n_stokes = n_elements(stokes)

  ; distortion coefficients
  k1 = comp_initialize_readconfig('k1', date_dir, options, /float)
  k2 = comp_initialize_readconfig('k2', date_dir, options, /float)

  ; parse the date_dir to find the Julian date to use to switch era of constants
  year  = fix(strmid(date_dir, 0, 4))
  month = fix(strmid(date_dir, 4, 2))
  day   = fix(strmid(date_dir, 6, 2))
  jd    = julday(month, day, year, 0, 0, 0)

  ; Era-specific correction factors

  ; offset of occulter post (pixels), positive shifts post clockwise in mask
  post_rotation = comp_initialize_readconfig('post_rotation', date_dir, options, /float)

  ; over or undersize occulter mask
  occulter_offset = comp_initialize_readconfig('occulter_offset', date_dir, options, /float)

  ; over or undersize field mask
  field_offset = comp_initialize_readconfig('field_offset', date_dir, options, /float)

  ; overlap of two beams, creating "ears"
  field_overlap = comp_initialize_readconfig('field_overlap', date_dir, options, /float)

  ; arcsec per pixel
  plate_scale = comp_initialize_readconfig('plate_scale', date_dir, options, /float)

  obj_destroy, options
end
