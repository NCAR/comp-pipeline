; docformat = 'rst'

pro comp_compare_backgrounds, date, dir
  compile_opt strictarr
  @comp_config_common

  files = file_search(filepath('*comp*.fts.gz', $
                               subdir=comp_decompose_date(date), $
                               root=archive_dir), $
                      count=n_files)

  re = '[[:digit:]]{8}\.[[:digit:]]{6}\.comp\.1074(\.[iquv]{1,4})?(\.[[:digit:]]{1,2})?\.fts\.gz'
  matches = stregex(file_basename(files), re, /boolean)
  l1_files = files[where(matches gt 0L, /null, n_l1_files)]
  l1_files = file_basename(l1_files)

  norm = comp_transmission(date)

  standard_backgrounds = fltarr(n_l1_files) + !values.f_nan
  compare_backgrounds = fltarr(n_l1_files) + !values.f_nan
  background_ratios = fltarr(n_l1_files) + !values.f_nan
  for f = 0L, n_l1_files - 1L do begin
    datetime = strmid(l1_files[f], 0, 15)
    standard_filename = filepath(l1_files[f], subdir=comp_decompose_date(date), root=archive_dir)

    compare_filename = filepath(datetime + '.*.fts.gz', subdir=[date, 'level1'], root=dir)
    candidates = file_search(compare_filename)
    matches = stregex(file_basename(candidates), '^' + strmid(l1_files[f], 0, 15) + '\.comp\.[[:digit:]]{4}\.[iquv]+\.[[:digit:]]{1,2}\.fts\.gz', /boolean)
    compare_filename = candidates[where(matches, /null)]
    if (n_elements(compare_filename) eq 1 && file_test(compare_filename) && file_test(standard_filename)) then begin
      fits_open, standard_filename, fcb
      fits_read, fcb, data, primary_header, /header_only, exten_no=0
      fits_close, fcb
      standard_background = sxpar(primary_header, 'BACKGRND')
      standard_backgrounds[f] = standard_background

      fits_open, compare_filename, fcb
      fits_read, fcb, data, primary_header, /header_only, exten_no=0
      fits_close, fcb
      compare_background = sxpar(primary_header, 'LCBKG')
      compare_backgrounds[f] = compare_background

      background_ratios[f] = compare_background / standard_background

      ; print, datetime, $
      ;        compare_background, $
      ;        standard_background, $
      ;        compare_background / standard_background, $
      ;        norm / 84.0, $
      ;        format='(%"%s: %6.3f, %6.3f, %0.3f, %0.3f")'
    endif
  endfor
  print, median(standard_backgrounds), $
         median(compare_backgrounds), $
         mean(background_ratios, /nan), $
         median(background_ratios), $
         format='(%"standard median bkg: %0.3f, compare median bkg: %0.3f, daily ratio mean: %0.3f, median: %0.3f")'
end


; main-level example program

config_filename = filepath('comp.production.cfg', $
                           subdir=['..', 'config'], $
                           root=mg_src_root())

; date = '20130612'
; transmission ratio 0.572
; process_dir = '/hao/dawn/Data/CoMP/process.reprocess-check'
; daily ratio mean: 0.589, median: 0.589

; date = '20130612'
; transmission ratio 0.572
; process_dir = '/hao/dawn/Data/CoMP/process.reprocess-check-nooptions'
; daily ratio mean: 0.698, median: 0.589


; date = '20140614'
; transmission ratio 0.501
; process_dir = '/hao/dawn/Data/CoMP/process.reprocess-check'
; daily ratio mean: 0.515, median: 0.515

; date = '20140614'
; transmission ratio 0.501
; process_dir = '/hao/dawn/Data/CoMP/process.reprocess-check-nooptions'
; daily ratio mean: 0.614, median: 0.615


; date = '20140618'
; transmission ratio 0.500
; process_dir = '/hao/dawn/Data/CoMP/process.reprocess-check'
; daily ratio mean: 0.517, median: 0.516


; date = '20140618'
; transmission ratio 0.500
; process_dir = '/hao/dawn/Data/CoMP/process.reprocess-check-nooptions'
; daily ratio mean: 0.608, median: 0.608


; date = '20140820'
; transmission ratio 0.488
; process_dir = '/hao/dawn/Data/CoMP/process.reprocess-check'
; daily ratio mean: 0.521, median: 0.526

; date = '20140820'
; transmission ratio 0.488
; process_dir = '/hao/dawn/Data/CoMP/process.reprocess-check-nooptions'
; daily ratio mean: 0.684, median: 0.710


; date = '20141118'
; transmission ratio 0.470
; process_dir = '/hao/dawn/Data/CoMP/process.reprocess-check'
; daily ratio mean: 0.457, median: 0.470

; date = '20141118'
; transmission ratio 0.470
; process_dir = '/hao/dawn/Data/CoMP/process.reprocess-check-nooptions'
; daily ratio mean: 0.541, median: 0.554


;dates = ['20130612', '20140614', '20140618', '20140820', '20141118']
dates = ['20170102', '20170819', '20170820']
for d = 0L, n_elements(dates) - 1L do begin
  comp_initialize, dates[d]
  comp_configuration, config_filename=config_filename

  ;process_dir = '/hao/dawn/Data/CoMP/process.reprocess-check'
  process_dir = '/hao/dawn/Data/CoMP/process.reprocess-check-nooptions'
  print, dates[d], format='(%"%s:")'
  comp_compare_backgrounds, dates[d], process_dir
endfor

end
