; docformat = 'rst'

;+
; Convert string date/time in UT to decimal hours in HST.
;
; :Returns:
;    `fltarr` of decimal hours in HST
;
; :Params:
;   times : in, required, type=strarr
;     date/time in the form 'YYYY-MM-DD HH:MM:SS' in UT
;-
function comp_plot_gbu_convert_times, times
  compile_opt strictarr

  hrs  = long(strmid(times, 11, 2))
  mins = long(strmid(times, 14, 2))
  secs = long(strmid(times, 17, 2))

  hrs -= 10                 ; convert from UT to HST
  hrs = (hrs + 24) mod 24   ; fix negative hrs

  hst_times = float(hrs) + mins / 60.0 + secs / 60.0 / 60.0

  return, hst_times
end


;+
; Make a histogram plot of the raw files from the day, color coded by quality
; type.
;
; Note: files that are not "OK" could be counted multiple times in the histogram.
;
; :Params:
;   date : in, required, type=string
;     observing date in the form "YYYYMMDD"
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079', or '1083'
;   output_filename : in, required, type=string
;     filename for output PNG
;   gbu_filename : in, required, type=string
;     filename for GBU file
;
; :Keywords:
;   _extra : in, optional, type=keywords
;     keywords to COMP_READ_GBU
;-
pro comp_plot_gbu, date, wave_type, output_filename, gbu_filename, _extra=e
  compile_opt strictarr

  mg_log, 'producing end-of-day GBU plot...', name='comp', /info

  type_bitmask = [0, 1, 2, 4, 8, 16, 32, 64, 128] 
  type_names = ['ok', 'dne', '3pt', 'mbhi', 'mblo', 'stdhi', 'bch', 'bhi', 'nan']
  n_types = n_elements(type_names)

  ; create quality histogram

  start_time = 06   ; 24-hour time
  end_time   = 19   ; 24-hour time
  increment  = 15   ; minutes
  max_images = 2 * increment   ; fastest cadence is 2 images/min
  n_bins = long((end_time  - start_time) / (increment / 60.0))

  histograms = lonarr(n_types, n_bins)

  gbu = comp_read_gbu(gbu_filename, count=n_files, _extra=e)

  if (n_files gt 0L) then begin
    for t = 0L, n_types - 1L do begin
      if (t eq 0) then begin
        ind = where(gbu.reason eq type_bitmask[t], n_type)
      endif else begin
        ind = where(gbu.reason and type_bitmask[t], n_type)
      endelse
      if (n_type gt 0L) then begin
        histograms[t, *] = histogram(comp_plot_gbu_convert_times((gbu.time_obs)[ind]), $
                                     min=start_time, $
                                     max=end_time - increment / 60.0, $
                                     nbins=n_bins, $
                                     locations=locations)
      endif
    endfor
  endif

  ; display plot

  original_device = !d.name
  set_plot, 'Z'
  device, set_resolution=[600, 120], set_pixel_depth=24, decomposed=1
  tvlct, original_rgb, /get

  colors = ['00a000'x, $   ; ok
            '00d0ff'x, $   ; dne
            'a06000'x, $   ; 3pt
            'a9a9a9'x, $   ; mbhi
            'e6d8ad'x, $   ; mblo
            '606060'x, $   ; stdhi
            '0090d0'x, $   ; bch
            'ee82ee'x, $   ; bhi
            '000000'x]     ; nan
  sums = total(histograms, 2, /preserve_type)
  mg_stacked_histplot, (increment / 60.0) * findgen(n_bins) + start_time, $
                       histograms, $
                       axis_color='000000'x, $
                       background='ffffff'x, color=colors, /fill, $
                       xstyle=9, xticks=end_time - start_time, xminor=4, $
                       ystyle=9, yrange=[0, max_images], yticks=4, $
                       charsize=0.85, $
                       title=string(date, wave_type, format='(%"%s (%s nm)")'), $
                       xtitle='Time (HST)', ytitle='# of images', $
                       position=[0.075, 0.25, 0.85, 0.80]

  square = mg_usersym(/square, /fill)
  mg_legend, item_name=type_names + ' ' + strtrim(sums, 2), $
             item_color=colors, $
             item_psym=square, $
             item_symsize=1.5, $
             color='000000'x, $
             charsize=0.85, $
             position=[0.875, 0.10, 0.95, 0.95]

  im = tvrd(true=1)
  tvlct, original_rgb
  set_plot, original_device

  ; make directory for output file, if it doesn't already exist
  dir_name = file_dirname(output_filename)
  if (~file_test(dir_name, /directory)) then file_mkdir, dir_name

  write_png, output_filename, im
end


; main-level example program

year = '2018'
month = '01'
wave_type = '1074'

for day = 1, 1 do begin
  date = string(year, month, day, format='(%"%s%s%02d")')
  l1_dir = string(date, format='(%"/hao/mahidata1/Data/CoMP/process/%s/level1")')
  gbu_filename = filepath(string(wave_type, format='(%"GBU.%s.log")'), root=l1_dir)
  if (~file_test(gbu_filename, /regular)) then continue

  output_filename = filepath(string(date, wave_type, $
                                    format='(%"%s.comp.%s.gbu.png")'), $
                             root='.')
  comp_plot_gbu, date, wave_type, output_filename, gbu_filename, n_header_lines=1
endfor

end
