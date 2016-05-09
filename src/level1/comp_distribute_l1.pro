; docformat = 'rst'

;+
; Make tarballs for and distribute CoMP Level_1 files from processing pipeline
; into the appropriate directories.
;
; :Examples:
;   For example::
;
;     comp_distribute_l1, '20121209', '1074'
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :Author:
;   Sitongia
;
; :Requires:
;   IDL 8.2.3
;-
pro comp_distribute_l1, date_dir, wave_type
  compile_opt strictarr
  @comp_config_common
  @comp_constants_common

  mg_log, 'distribute L1 for %s', wave_type, name='comp', /info

  l1_process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)
  cd, l1_process_dir

  ; for the directory name
  year  = strmid(date_dir, 0, 4)
  month = strmid(date_dir, 4, 2)
  day   = strmid(date_dir, 6, 4)

  adir  = filepath('', subdir=[year, month, day], root=archive_dir)
  frdir = filepath('', subdir=[year, month, day], root=fullres_dir)

  ; prepare directories for level 1 files
  if (~file_test(adir, /directory)) then file_mkdir, adir
  file_chmod, adir, /g_write

  if (~file_test(frdir, /directory)) then file_mkdir, frdir
  file_chmod, frdir, /g_write

  ; copy ALL FITS files to archive

  mg_log, 'copying FITS files...', name='comp', /info
  l1_files = comp_find_l1_file(date_dir, wave_type, /all, $
                               count=n_l1_files)
  if (n_l1_files gt 0L) then file_copy, l1_files, adir, /overwrite

  ; copy all the .gifs, not just the good ones
  mg_log, 'copying GIF files...', name='comp', /info
  file_copy, '*.comp.' + wave_type + '.intensity.gif', frdir, /overwrite

  ; save the GBU file
  mg_log, 'copying GBU file...', name='comp', /info

  gbu_dir = filepath('', subdir=['GBU', year], root=log_dir)
  file_mkdir, gbu_dir
  file_copy, 'GBU.' + wave_type + '.log', $
             filepath(date_dir + '.GBU.' + wave_type + '.log', root=gbu_dir), $
             /overwrite

  ; tar and send to HPSS

  l1_tarname = date_dir + '.comp.' + wave_type + '.l1.tgz'
  mg_log, 'tarring L1 results in %s', l1_tarname, name='comp', /info

  tar_list = [['cal', 'dark', 'opal'] + '_files.txt', $
              ['', 'good_', 'good_all_', 'good_waves_', 'synoptic_'] $
                + wave_type + '_files.txt', $
              'GBU.' + wave_type + '.log', $
              'flat.fts', 'dark.fts']

  ; SPAWN-ing tar seems to be about 10-15% faster than FILE_TAR
  idl_tar = 1B
  if (idl_tar) then begin
    if (n_l1_files gt 0L) then tar_list = [tar_list, l1_files]
    if (n_l1_bkg_files gt 0L) then tar_list = [tar_list, l1_bkg_files]

    file_tar, tar_list, l1_tarname, /gzip
  endif else begin
    spawn, string(l1_tarname, wave_type, strjoin(tar_list, ' '), $
                  format='(%"tar cfz %s *.comp.%s*.fts %s")')
  endelse

  if (send_to_hpss) then begin
    mg_log, 'linking to L1 tarball from HPSS dir...', name='comp', /info
    if (~file_test(hpss_gateway, /directory)) then file_mkdir, hpss_gateway
    file_link, filepath(l1_tarname, root=l1_process_dir), hpss_gateway
  endif else begin
    mg_log, 'skipping linking to L1 tarball from HPSS dir...', name='comp', /info
  endelse

  mg_log, 'done', name='comp', /info
end
