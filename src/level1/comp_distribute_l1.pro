; docformat = 'rst'

;+
; Make tarballs for and distribute CoMP Level 1 files from processing pipeline
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
;   wave_type : in, optional, type=string
;     wavelength range for the observations, '1074', '1079' or '1083';
;     distribute wavelength independent files such as flats and darks if not
;     provided
;
; :Author:
;   MLSO Software Team
;
; :Requires:
;   IDL 8.2.3
;-
pro comp_distribute_l1, date_dir, wave_type
  compile_opt strictarr
  @comp_config_common
  @comp_constants_common

  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    mg_log, /last_error, name='comp'
    goto, done
 endif

  l1_process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)
  cd, l1_process_dir

  ; for the directory name
  ymd = comp_decompose_date(date_dir)

  eng_dir = filepath('', subdir=ymd, root=engineering_dir)
  if (~file_test(eng_dir, /directory)) then file_mkdir, eng_dir

  ; save the flats and darks doing a wave_type independent call
  if (n_elements(wave_type) eq 0L) then begin
    mg_log, 'distribute L1 flats and darks', name='comp', /info
    file_copy, 'dark.fts', eng_dir, /overwrite
    file_copy, 'flat.fts', eng_dir, /overwrite
    mg_log, 'done', name='comp', /info
    return
  endif

  mg_log, 'distribute L1 for %s', wave_type, name='comp', /info

  adir  = filepath('', subdir=ymd, root=archive_dir)
  frdir = filepath('', subdir=ymd, root=fullres_dir)

  ; prepare directories for level 1 files
  if (~file_test(adir, /directory)) then file_mkdir, adir
  if (~file_test(adir, /group, /write)) then file_chmod, adir, /g_write

  if (~file_test(frdir, /directory)) then file_mkdir, frdir
  if (~file_test(frdir, /group, /write)) then file_chmod, frdir, /g_write

  ; copy L1 FITS files to archive
  mg_log, 'copying FITS files...', name='comp', /info
  l1_files = comp_find_l1_file(date_dir, wave_type, /all, $
                               count=n_l1_files)
  if (n_l1_files gt 0L) then file_copy, l1_files, adir, /overwrite

  ; copy all the .gifs, not just the good ones
  mg_log, 'copying GIF files...', name='comp', /info
  gif_wildcard = '*.comp.' + wave_type + '.intensity.gif'
  gif_files = file_search(gif_wildcard, count=n_gif_files)
  if (n_gif_files gt 0L) then file_copy, gif_wildcard, frdir, /overwrite

  ; save the GBU file
  mg_log, 'copying GBU file...', name='comp', /info

  gbu_filename = string(date_dir, wave_type, format='(%"%s.comp.%s.gbu.log")')
  if (file_test(gbu_filename)) then begin
    file_copy, gbu_filename, $
               filepath(gbu_filename, root=eng_dir), $
               /overwrite
  endif

  ; save and distribute the .txt files
  mg_log, 'copying .txt files...', name='comp', /info
  txt_files = file_search('*' + wave_type + '*.txt', count=n_txt_files)
  if (n_txt_files gt 0L) then begin
    file_copy, txt_files, adir, /overwrite
    file_copy, txt_files, eng_dir, /overwrite
  endif else begin
    mg_log, 'no text files to distribute for wave type %s', wave_type, $
            name='comp', /warn
  endelse

  ; tar and send to HPSS
  if (send_to_hpss) then begin
    mg_log, 'tarring and sending L1 for %s to HPSS', wave_type, name='comp', /info
    if (~file_test(hpss_gateway, /directory)) then file_mkdir, hpss_gateway

    time_delay = '0h'
    background_indicator = wave_type eq '1083' ? '' : '&'
    archive_script = filepath('archive_l1.sh', $
                              subdir=['..', 'scripts'], $
                              root=binary_dir)
    cmd = string(archive_script, $
                 date_dir, wave_type, hpss_gateway, time_delay, $
                 background_indicator, $
                 format='(%"%s %s %s %s %s %s")')

    spawn, cmd, result, error_result, exit_status=status
    if (status ne 0L) then begin
      mg_log, 'problem sending data to HPSS with command: %s', cmd, $
              name='comp', /error
      mg_log, '%s', error_result, name='comp', /error
      goto, done
    endif

    ; send 1083 L1 tarball to the archive
    if (wave_type eq '1083') then begin
      tarball_filename = string(date_dir, wave_type, $
                                format='(%"%s.comp.%s.l1.tgz")')
      mg_log, 'distribute %s tarball to archive', wave_type, name='comp', /info
      file_copy, tarball_filename, adir, /overwrite
    endif
  endif else begin
    mg_log, 'skipping linking to L1 tarball from HPSS dir...', name='comp', /info
  endelse

  done:
  mg_log, 'done', name='comp', /info
end
