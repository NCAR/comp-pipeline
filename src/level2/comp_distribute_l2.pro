; docformat = 'rst'

;+
; Distribute CoMP Level_2 files from processing pipeline into the appropriate
; directories.
;
; :Examples:
;   Try::
;
;     comp_distribute_l2, '20121209', '1074'
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :Author:
;   Sitongia
;-
pro comp_distribute_l2, date_dir, wave_type
  compile_opt strictarr
  @comp_config_common
  @comp_constants_common

  mg_log, 'distribute L2 for %s', wave_type, name='comp', /info
  
  process_dir = filepath('', subdir=date_dir, root=process_basedir)
  cd, process_dir
  
  ; for the directory name
  year = strmid(date_dir, 0, 4)
  month = strmid(date_dir, 4, 2)
  day = strmid(date_dir, 6, 4)
  destination = year + '/' + month + '/' + day  

  adir = filepath('', subdir=destination, root=archive_dir)
  frdir = filepath('', subdir=destination, root=fullres_dir)

  ; copy files
  mg_log, 'copying mean and quick_invert files...', name='comp', /info
  file_copy, date_dir + '.comp.' + wave_type + '.mean.fts.gz', adir, /overwrite
  file_copy, date_dir + '.comp.' + wave_type + '.quick_invert.fts.gz', adir, /overwrite

  types = ['dynamics', 'polarization', 'daily_dynamics', 'daily_polarization']
  for t = 0L, n_elements(types) - 1L do begin
    mg_log, 'copying %s FITS files...', types[t], name='comp', /info
    files = file_search('*.comp.' + wave_type + '.' + types[t] + '.*.ftz.gz', count=count)
    if (count gt 0L) then file_copy, files, adir, /overwrite
  endfor

  file_copy, date_dir + '.comp.' + wave_type + '.daily_summary.txt', adir, /overwrite

  mg_log, 'copying quick view images...', name='comp', /info
  files = file_search('*.comp.' + wave_type + '.quickview*.jpg', count=count)
  if (count gt 0L) then file_copy, files, frdir, /overwrite  

  cd, filepath('', subdir='movies', root=process_dir)

  ; PNGs
  mg_log, 'copying PNGs...', name='comp', /info
  files = file_search('*.comp.' + wave_type + '.daily*.png', count=count)
  if (count gt 0L) then file_copy, files, frdir, /overwrite

  ; JPGs
  mg_log, 'copying JPGs...', name='comp', /info
  files = file_search('*.comp.' + wave_type + '.daily*.jpg', count=count)
  if (count gt 0L) then file_copy, files, frdir, /overwrite

  ; movies
  mg_log, 'copying movies...', name='comp', /info
  files = file_search('*.comp.' + wave_type + '*.mp4', count=count)
  if (count gt 0L) then file_copy, files, frdir, /overwrite

  cd, '..'

  ; tar and send to HPSS

  l2_tarname = date_dir + '.comp.' + wave_type + '.l2.tgz'

  mg_log, 'tar results', name='comp', /info
  ; TODO: check the performance of this command vs SPAWNing tar
  types = ['mean', 'quick_invert']
  file_tar, date_dir + '.comp.' + wave_type + '.' + types + '.fts.gz', $
            l2_tarname, $
            /gzip
  ;spawn, 'tar czf ' + l2_tarname + ' ' +  $
  ;  date_dir + '.comp.' + wave_type + '.mean.fts.gz' + ' ' + $
  ;  date_dir + '.comp.' + wave_type + '.quick_invert.fts.gz'

  mg_log, 'send to HPSS', name='comp', /info
  file_link, filepath(l2_tarname, root=process_dir), hpss_gateway
  
  mg_log, 'done', name='comp', /info
end
