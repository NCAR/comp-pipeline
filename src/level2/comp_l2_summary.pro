; docformat = 'rst

;+
;
; :Uses:
;   comp_config_common
;   mg_log
;
; :Params:
;   date_dir : in, required, type=string
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
pro comp_l2_summary, date_dir, wave_type
  compile_opt strictarr
  @comp_config_common

  mg_log, 'wave_type: %s', wave_type, name='comp', /info

  l1_process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)
  l2_process_dir = filepath('', subdir=[date_dir, 'level2'], root=process_basedir)
  cd, l2_process_dir

  ; for the directory name
  year = strmid(date_dir, 0, 4)
  month = strmid(date_dir, 4, 2)
  day = strmid(date_dir, 6, 4)
  destination = string(year, month, day, format='(%"/%s/%s/%s/")')

  ; get some info
  flist = file_basename(comp_find_l1_file(date_dir, wave_type, /all, count=n_files))
  if (n_files eq 0L) then begin
    start_time  = '00:00:00'
    end_time    = '00:00:00'
  endif else begin
    start_dt = strmid(flist[0], 9, 6)
    start_time = string(strmid(start_dt, 0, 2), $
                        strmid(start_dt, 2, 2), $
                        strmid(start_dt, 4, 2), $
                        format='(%"%s:%s:%s UT")')
    end_dt = strmid(flist[n_elements(flist) - 1], 9, 6)
    end_time = string(strmid(end_dt, 0, 2), $
                      strmid(end_dt, 2, 2), $
                      strmid(end_dt, 4, 2), $
                      format='(%"%s:%s:%s UT")')
  endelse

  mg_log, '%d files going into info and tar for %s to %s', $
          n_files, start_time, end_time, name='comp', /info

  flist_d = file_search('*.comp.' + wave_type + '.dynamics.fts.gz')
  flist_p = file_search('*.comp.' + wave_type + '.polarization.fts.gz')
  if (n_elements(flist_d) eq 1 and flist_d[0] eq '') then begin
    n_dynamics_files = 0
  endif else begin
    n_dynamics_files = n_elements(flist_d)
  endelse

  ; write out info file
  mg_log, 'writing out info file...', name='comp', /info

  openw,  funit, date_dir + '.comp.' + wave_type + '.daily_summary.txt', /get_lun
  printf, funit, 'Start ' + start_time
  printf, funit, 'End ' + end_time
  printf, funit, n_dynamics_files, format='(%"Number_of_files %d")'

  if (not (n_elements(flist_d) eq 1 and flist_d[0] eq '')) then begin
    for i = 0L, n_elements(flist_d) - 1L do begin
      printf, funit, archive_dir, destination, flist_d[i], $
              format='(%"http://mlso.hao.ucar.edu%s%s%s")'
    endfor
    for i = 0L, n_elements(flist_p) - 1L do begin
      printf, funit, archive_dir, destination, flist_p[i], $
              format='(%"http://mlso.hao.ucar.edu%s%s%s")'
    endfor
  endif

  free_lun, funit

  mg_log, 'done', name='comp', /info
end
