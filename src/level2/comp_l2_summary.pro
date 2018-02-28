; docformat = 'rst

;+
;
; :Uses:
;   comp_config_common
;   mg_log
;
; :Params:
;   date : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :Author:
;   MLSO Software Team
;
; :History:
;   removed gzip    Oct 1 2014  GdT
;   removed tarball creation - left list of dynamics files    Oct 1 2014  GdT
;   see git log for recent changes
;-
pro comp_l2_summary, date, wave_type
  compile_opt strictarr
  @comp_config_common

  mg_log, 'wave_type: %s nm', wave_type, name='comp', /info

  l2_process_dir = filepath('', subdir=[date, 'level2'], root=process_basedir)
  cd, l2_process_dir

  ; for the directory name

  destination = strjoin(comp_decompose_date(date), '/')

  ; get some info
  l1_files = comp_find_l1_file(date, wave_type, /all, count=n_l1_files)
  if (n_l1_files eq 0L) then begin
    start_time  = '00:00:00'
    end_time    = '00:00:00'
  endif else begin
    l1_basenames = file_basename(l1_files)
    start_dt = strmid(l1_basenames[0], 9, 6)
    start_time = string(strmid(start_dt, 0, 2), $
                        strmid(start_dt, 2, 2), $
                        strmid(start_dt, 4, 2), $
                        format='(%"%s:%s:%s UT")')
    end_dt = strmid(l1_basenames[n_l1_files - 1], 9, 6)
    end_time = string(strmid(end_dt, 0, 2), $
                      strmid(end_dt, 2, 2), $
                      strmid(end_dt, 4, 2), $
                      format='(%"%s:%s:%s UT")')
  endelse

  dynamics_files = file_search('*.comp.' + wave_type + '.dynamics.fts.gz', $
                               count=n_dynamics_files)
  polarization_files = file_search('*.comp.' + wave_type + '.polarization.fts.gz', $
                                   count=n_polarization_files)

  mg_log, '%d L1 files for info and tar files', $
          n_l1_files, name='comp', /info
  mg_log, '%d dynamics files, %d polarization files', $
          n_dynamics_files, n_polarization_files, name='comp', /info
  mg_log, '%s to %s', $
          start_time, end_time, name='comp', /info

  ; write out info file
  summary_filename = date + '.comp.' + wave_type + '.daily_summary.txt'
  mg_log, 'writing %s', summary_filename, name='comp', /info

  openw, lun, summary_filename, /get_lun

  printf, lun, 'Start ' + start_time
  printf, lun, 'End ' + end_time
  printf, lun, n_dynamics_files, format='(%"Number_of_files %d")'

  base_url = 'http://mlso.hao.ucar.edu'

  for f = 0L, n_dynamics_files - 1L do begin
    printf, lun, base_url, archive_dir, destination, dynamics_files[f], $
            format='(%"%s%s/%s/%s")'
  endfor
  for f = 0L, n_polarization_files - 1L do begin
    printf, lun, base_url, archive_dir, destination, polarization_files[f], $
            format='(%"%s%s/%s/%s")'
  endfor

  done:
  free_lun, lun
  mg_log, 'done', name='comp', /info
end


; main-level example program

date = '20180131'
config_filename = filepath('comp.mgalloy.mahi.latest.cfg', $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())
comp_configuration, config_filename=config_filename
comp_l2_summary, date, '1074'

end
