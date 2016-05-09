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
  
  l1_process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)
  l2_process_dir = filepath('', subdir=[date_dir, 'level2'], root=process_basedir)
  cd, l2_process_dir
  
  ; for the directory name
  year = strmid(date_dir, 0, 4)
  month = strmid(date_dir, 4, 2)
  day = strmid(date_dir, 6, 4)
  destination = year + '/' + month + '/' + day  

  adir = filepath('', subdir=destination, root=archive_dir)
  frdir = filepath('', subdir=destination, root=fullres_dir)

  ; copy files

  types = ['mean', 'quick_invert']
  for t = 0L, n_elements(types) - 1L do begin
    mg_log, 'copying %s files...', types[t], name='comp', /info
    filename = string(date_dir, wave_type, types[t], format='(%"%s.comp.%s.%s.fts")')
    if (file_test(filename)) then file_copy, filename, adir, /overwrite
  endfor

  types = ['dynamics', 'polarization', 'daily_dynamics', 'daily_polarization']
  for t = 0L, n_elements(types) - 1L do begin
    files = file_search('*.comp.' + wave_type + '.' + types[t] + '.*.fts', $
                        count=n_files)
    mg_log, 'copying %d %s FITS files...', n_files, types[t], name='comp', /info
    if (n_files gt 0L) then file_copy, files, adir, /overwrite
  endfor

  file_copy, date_dir + '.comp.' + wave_type + '.daily_summary.txt', adir, /overwrite

  files = file_search('*.comp.' + wave_type + '.quickview*.jpg', count=n_files)
  mg_log, 'copying %d quick view images...', n_files, name='comp', /info
  if (n_files gt 0L) then file_copy, files, frdir, /overwrite  

  cd, filepath('', subdir='movies', root=l2_process_dir)

  ; PNGs
  files = file_search('*.comp.' + wave_type + '.daily*.png', count=n_files)
  mg_log, 'copying %d PNGs...', n_files, name='comp', /info
  if (n_files gt 0L) then file_copy, files, frdir, /overwrite

  ; JPGs
  files = file_search('*.comp.' + wave_type + '.daily*.jpg', count=n_files)
  mg_log, 'copying %d JPGs...', n_files, name='comp', /info
  if (n_files gt 0L) then file_copy, files, frdir, /overwrite

  ; movies
  files = file_search('*.comp.' + wave_type + '*.mp4', count=n_files)
  mg_log, 'copying %d movies...', n_files, name='comp', /info
  if (n_files gt 0L) then file_copy, files, frdir, /overwrite

  cd, '..'

  ; tar and send to HPSS

  if (wave_type ne '1083') then begin
    l2_tarname = date_dir + '.comp.' + wave_type + '.l2.tgz'

    mg_log, 'tar results', name='comp', /info

    types = ['mean', 'quick_invert']
    tar_list = date_dir + '.comp.' + wave_type + '.' + types + '.fts'

    idl_tar = 1B
    if (idl_tar) then begin
      file_tar, tar_list, l2_tarname, /gzip
    endif else begin
      spawn, string(l2_tarname, strjoin(tar_list, ' '), $
                    format='(%"tar cfz %s %s")')
    endelse

    if (send_to_hpss) then begin
      mg_log, 'linking to L2 tarball from HPSS dir...', name='comp', /info
      if (~file_test(hpss_gateway, /directory)) then file_mkdir, hpss_gateway
      l2_tarball = filepath(l2_tarname, root=hpss_gateway)
      if (file_test(l2_tarball, /symlink) $
            || file_test(l2_tarball, /dangling_symlink)) then begin
        mg_log, 'removing old link to %s', l2_tarname, name='comp', /warning
        file_delete, l2_tarball
      endif
      file_link, filepath(l2_tarname, root=l2_process_dir), hpss_gateway
    endif else begin
      mg_log, 'skipping linking to L2 tarball from HPSS dir...', name='comp', /info
    endelse
  endif

  mg_log, 'done', name='comp', /info
end
