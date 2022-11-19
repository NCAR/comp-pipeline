; docformat = 'rst'

;+
; Create a synoptic plot for the last 28 days.
;
; :Params:
;   wave_region : in, required, type=string
;     wave region to produce a synoptic map for 
;   height : in, required, type=float
;     height of annulus +/- 0.02 Rsun [Rsun]
;   field : in, required, type=string
;     field in ucomp_sci database table to retrieve data from
;   db : in, required, type=object
;     database connection
;
; :Keywords:
;   run : in, required, type=object
;     KCor run object
;-
pro comp_synoptic_map, wave_region, $
                       start_date, end_date, $
                       name, height, field, db
  compile_opt strictarr

  logger_name = 'comp/analysis'

  start_date_tokens = comp_decompose_date(start_date)
  _start_date = strjoin(start_date_tokens, '-')
  end_date_tokens = comp_decompose_date(end_date)
  _end_date = strjoin(end_date_tokens, '-')
  end_date_jd = julday(end_date_tokens[1], end_date_tokens[2], end_date_tokens[0])
  start_date_jd = julday(start_date_tokens[1], start_date_tokens[2], start_date_tokens[0])
  n_days =  end_date_jd - start_date_jd

  mg_log, 'producing synoptic map from %s to %s (%d days)', $
          _start_date, _end_date, n_days, $
          name=logger_name, /info


  query = 'select comp_sci.date_obs, comp_sci.%s from comp_sci, mlso_numfiles where comp_sci.file_name like \"%%.%s.%%\" and comp_sci.obs_day=mlso_numfiles.day_id and mlso_numfiles.obs_day between ''%s'' and ''%s'''
  raw_data = db->query(query, field, wave_region, _start_date, _end_date, $
                       count=n_rows, error=error, fields=fields, sql_statement=sql)
  if (n_rows gt 0L) then begin
    mg_log, '%d dates between %s and %s', n_rows, _start_date, _end_date, $
            name=logger_name, /debug
  endif else begin
    mg_log, 'no data found between %s and %s', _start_date, _end_date, $
            name=logger_name, /warn
    goto, done
  endelse

  ; organize data
  product_data = raw_data.(1)

  dates = raw_data.date_obs
  n_dates = n_elements(dates)

  map = fltarr(n_days, 720) + !values.f_nan
  means = fltarr(n_days) + !values.f_nan
  for r = 0L, n_dates - 1L do begin
    decoded = *product_data[r]
    if (n_elements(decoded) gt 0L) then begin
      *product_data[r] = float(*product_data[r], 0, 720)   ; decode byte data to float
    endif

    date = dates[r]
    date_index = ucomp_dateobs2julday(date) - start_date_jd - 10.0/24.0
    date_index = floor(date_index)

    if (ptr_valid(product_data[r]) && n_elements(*product_data[r]) gt 0L) then begin
      map[date_index, *] = *product_data[r]
      means[date_index] = mean(*product_data[r])
    endif else begin
      map[date_index, *] = !values.f_nan
      means[date_index] = !values.f_nan
    endelse
  endfor

  ; configure device

  original_device = !d.name

  set_plot, 'Z'
  device, set_resolution=[(30 * n_days + 50) < 1200, 800]

  device, get_decomposed=original_decomposed
  device, decomposed=0

  n_colors = 253
  ucomp_loadct, 'intensity', n_colors=n_colors

  display_gamma = 0.7
  gamma_ct, display_gamma, /current

  display_min   = 0.1
  display_max   = name eq 'background' ? 50.0 : 12.0
  display_power = 0.7

  background_color = 253
  tvlct, 255, 255, 255, background_color
  text_color = 254
  tvlct, 0, 0, 0, text_color
  detail_text_color = 255
  tvlct, 128, 128, 128, detail_text_color

  tvlct, rgb, /get

  nan_indices = where(finite(map) eq 0, n_nan)

  map = bytscl(map^display_power, $
               min=display_min^display_power, $
               max=display_max^display_power, $
               top=n_colors - 1L, $
               /nan)

  if (n_nan gt 0L) then map[nan_indices] = 0

  map = float(map)

  north_up_map = shift(map, 0, -180)
  east_limb = reverse(north_up_map[*, 0:359], 2)
  west_limb = north_up_map[*, 360:*]

  !null = label_date(date_format='%D %M %Z')
  jd_dates = dblarr(n_dates)
  for d = 0L, n_dates - 1L do jd_dates[d] = ucomp_dateobs2julday(dates[d])

  charsize = 0.9
  ;smooth_kernel = [11, 1]
  ;smooth_kernel = [3, 1]

  title = string(name, wave_region, height, _start_date, _end_date, $
                 format='(%"CoMP synoptic map for %s at %s nm at r%0.2f from %s to %s")')
  erase, background_color
  mg_image, reverse(east_limb, 1), reverse(jd_dates), $
            xrange=[end_date_jd, start_date_jd], $
            xtyle=1, xtitle='Date (not offset for E limb)', $
            min_value=0.0, max_value=255.0, $
            /axes, yticklen=-0.005, xticklen=-0.01, $
            color=text_color, background=background_color, $
            title=string(title, format='(%"%s (East limb)")'), $
            xtickformat='label_date', $
            position=[0.05, 0.55, 0.97, 0.95], /noerase, $
            yticks=4, ytickname=['S', 'SE', 'E', 'NE', 'N'], yminor=4, $
            smooth_kernel=smooth_kernel, $
            charsize=charsize
  mg_image, reverse(west_limb, 1), reverse(jd_dates), $
            xrange=[end_date_jd, start_date_jd], $
            xstyle=1, xtitle='Date (not offset for W limb)', $
            min_value=0.0, max_value=255.0, $
            /axes, yticklen=-0.005, xticklen=-0.01, $
            color=text_color, background=background_color, $
            title=string(title, format='(%"%s (West limb)")'), $
            xtickformat='label_date', $
            position=[0.05, 0.05, 0.97, 0.45], /noerase, $
            yticks=4, ytickname=['S', 'SW', 'W', 'NW', 'N'], yminor=4, $
            smooth_kernel=smooth_kernel, $
            charsize=charsize

  xyouts, 0.97, 0.49, /normal, alignment=1.0, $
          string(display_min, display_max, display_power, $
                 format='(%"min/max/exp: %0.1f, %0.1f, %0.2f")'), $
          charsize=charsize, color=detail_text_color

  im = tvrd()

  eng_dir = '.'

  gif_filename = filepath(string(wave_region, $
                                 name, $
                                 100.0 * height, $
                                 format='(%"comp.%s.synoptic.%s.r%03d.gif")'), $
                          root=eng_dir)
  write_gif, gif_filename, im, rgb[*, 0], rgb[*, 1], rgb[*, 2]

;   mkhdr, primary_header, map, /extend
;   sxdelpar, primary_header, 'DATE'
;   ucomp_addpar, primary_header, 'DATE-OBS', start_date, $
;                 comment='[UTC] start date of synoptic map', $
;                 after='EXTEND'
;   ucomp_addpar, primary_header, 'DATE-END', end_date, $
;                 comment='[UTC] end date of synoptic map', $
;                 format='(F0.2)', after='DATE-OBS'
;   ucomp_addpar, primary_header, 'PRODUCT', name, $
;                 comment='name of product', $
;                 after='DATE-END'
;   ucomp_addpar, primary_header, 'HEIGHT', height, $
;                 comment='[Rsun] height of annulus +/- 0.02 Rsun', $
;                 format='(F0.2)', after='DATE-END'
; 
;   fits_filename = filepath(string(run.date, $
;                                   wave_region, $
;                                   flag, $
;                                   100.0 * height, $
;                                   format='(%"%s.ucomp.%s.28day.synoptic.%s.r%03d.fts")'), $
;                            root=eng_dir)
;   writefits, fits_filename, map, primary_header

  ; clean up
  done:
  if (n_elements(rgb) gt 0L) then tvlct, rgb
  if (n_elements(original_decomposed) gt 0L) then device, decomposed=original_decomposed
  if (n_elements(original_device) gt 0L) then set_plot, original_device

  for d = 0L, n_elements(data) - 1L do begin
    s = raw_data[d]
    ptr_free, s.(1)
  endfor

  mg_log, 'done', name=logger_name, /info
end


; main-level example program

date = '20220930'

config_basename = 'ucomp.production.cfg'
config_filename = filepath(config_basename, $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())
db = compdbmysql()
db->connect, config_filename='~/.mysqldb', $
             config_section='mgalloy@databases', $
             status=status, error_message=error_message

comp_synoptic_map, '1074', '20121201', '20140501', 'intensity', 1.10, 'r11_intensity', db
;comp_synoptic_map, '1074', '20121201', '20180406', 'intensity', 1.10, 'r11_intensity', db
;comp_synoptic_map, '1074', '20121201', '20180406', 'intensity', 1.20, 'r12_intensity', db
;comp_synoptic_map, '1074', '20121201', '20180406', 'background', 1.11, 'r11_continuum', db

obj_destroy, db

end
