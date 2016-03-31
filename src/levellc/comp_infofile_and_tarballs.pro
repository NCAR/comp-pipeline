; docformat = 'rst

;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :History:
;    removed gzip    Oct 1 2014  GdT
;    removed tarball creation - left list of dynamics files    Oct 1 2014  GdT
;-
pro comp_infofile_and_tarballs, date_dir, wave_type
  compile_opt strictarr
  @comp_paths_common

  ; configure
  comp_initialize, date_dir
  comp_paths, date_dir

  mg_log, 'wave_type: %s', wave_type, name='comp', /info

  process_dir = filepath(date_dir, root=process_basedir)
  cd, process_dir

  ; for the directory name
  year = strmid(date_dir, 0, 4)
  month = strmid(date_dir, 4, 2)
  day = strmid(date_dir, 6, 4)
  destination = filepath('', subdir=[year, month, day], root='.')

  ; get some info
  flist  = file_search('*.comp.' + wave_type + '.fts')
  if (n_elements(flist) eq 1 and flist[0] eq '') then begin
    no_of_files = '0'
    start_time  = '00:00:00'
    end_time    = '00:00:00'
  endif else begin
    no_of_files = strcompress(string(n_elements(flist)), /remove_all)
    start_time  = strmid(flist[0], 19, 2, /reverse) $
                    + ':' + strmid(flist[0], 17, 2, /reverse) $
                    + ':' + strmid(flist[0], 15, 2, /reverse) $
                    + ' UT'
    end_time    = strmid(flist[n_elements(flist) - 1L], 19, 2, /reverse) $
                    + ':' + strmid(flist[n_elements(flist) - 1L], 17, 2, /reverse) $
                    + ':' + strmid(flist[n_elements(flist) - 1L], 15, 2, /reverse) $
                    + ' UT'
  endelse

  mg_log, '%d files going into info and tar for %s to %s', $
          no_of_files, start_time, end_time, name='comp', /info

  flist_d_tpt = file_search('*.comp.' + wave_type + '.dynamics.3.fts')
  flist_p_tpt = file_search('*.comp.' + wave_type + '.polarization.3.fts')
  if (n_elements(flist_d_tpt) eq 1 and flist_d_tpt[0] eq '') then begin
    no_of_tpt_files = '0'
  endif else begin
    no_of_tpt_files = strcompress(string(n_elements(flist_d_tpt)), /remove_all)
  endelse

  flist_d_fpt = file_search('*.comp.' + wave_type + '.dynamics.5.fts')
  flist_p_fpt = file_search('*.comp.' + wave_type + '.polarization.5.fts')
  if (n_elements(flist_d_fpt) eq 1 and flist_d_fpt[0] eq '') then begin
    no_of_fpt_files = '0'
  endif else begin
    no_of_fpt_files = strcompress(string(n_elements(flist_d_fpt)), /remove_all)
  endelse

  ; write out info file
  mg_log, 'writing out info file...', name='comp', /info

  openw,  funit, date_dir + '.comp.' + wave_type + '.daily_summary.txt', /get_lun
  printf, funit, 'Start ' + start_time
  printf, funit, 'End ' + end_time
  printf, funit, 'Number_of_3pt_files ' + no_of_tpt_files
  printf, funit, 'Number_of_5pt_files ' + no_of_fpt_files

  if (not (n_elements(flist_d_tpt) eq 1 and flist_d_tpt[0] eq '')) then begin
    for ii = 0L, n_elements(flist_d_tpt) - 1L do begin
      printf, funit, 'http://mlso.hao.ucar.edu' + archive_dir + destination $
                + strmid(flist_d_tpt[ii], 42, 43, /reverse_offset)
    endfor
    for ii = 0L, n_elements(flist_p_tpt) - 1L do begin
      printf, funit, 'http://mlso.hao.ucar.edu' + archive_dir + destination $
                + strmid(flist_p_tpt[ii], 46, 47, /reverse_offset)
    endfor
  endif
  if (not (n_elements(flist_d_fpt) eq 1 and flist_d_fpt[0] eq '')) then begin
    for ii = 0L, n_elements(flist_d_fpt) - 1L do begin
      printf, funit, 'http://mlso.hao.ucar.edu' + archive_dir + destination $
                + strmid(flist_d_fpt[ii], 42, 43, /reverse_offset)
    endfor
    for ii = 0L, n_elements(flist_p_fpt) - 1L do begin
      printf, funit, 'http://mlso.hao.ucar.edu' + archive_dir + destination $
                + strmid(flist_p_fpt[ii], 46, 47, /reverse_offset)
    endfor
  endif

  free_lun, funit

  ;create tarballs
  ;if not (n_elements(flist_d_tpt) eq 1 and flist_d_tpt[0] eq '') then begin
  ;  printf, logFileUnit, 'Creating 3pt dynamics tarball ...'
  ;  spawn, 'tar czf '+date_dir+'.comp.'+wave_type+$
  ;    '.daily_dynamics.3.tar.gz *.comp.'+wave_type+'.dynamics.3.fts.gz'
  ;  printf, logFileUnit, 'Creating 3pt polarization tarball ...'
  ;  spawn, 'tar czf '+date_dir+'.comp.'+wave_type+$
  ;    '.daily_polarization.3.tar.gz *.comp.'+wave_type+'.polarization.3.fts.gz'
  ;endif
  
  ;if not (n_elements(flist_d_fpt) eq 1 and flist_d_fpt[0] eq '') then begin
  ;  printf, logFileUnit, 'Creating 5pt dynamics tarball ...'
  ;  spawn, 'tar czf '+date_dir+'.comp.'+wave_type+$
  ;    '.daily_dynamics.5.tar.gz *.comp.'+wave_type+'.dynamics.5.fts.gz'
  ;  printf, logFileUnit, 'Creating 5pt polarization tarball ...'
  ;  spawn, 'tar czf '+date_dir+'.comp.'+wave_type+$
  ;    '.daily_polarization.5.tar.gz *.comp.'+wave_type+'.polarization.5.fts.gz'
  ;endif

  mg_log, 'done', name='comp', /info
end
