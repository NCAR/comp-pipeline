; docformat = 'rst'

;+
; Procedure to run calibrate_comp_wavelength on all available flat data.
;-
pro run_calibrate_comp_wavelength
  compile_opt strictarr

  dir = 'C:\Users\tomczyk\Documents\Comp\idl\Systematics\'
  cd, dir

  lam0 = 1074.7   ; wavelength region (nm) (1074.7 or 1079.8)
  ;lam0 = 1079.8

  openpost, dev, default='s', file='wavecal.ps'
  openw, lun, 'C:\Users\tomczyk\Documents\Comp\idl\Systematics\out.txt', /get_lun

  jd_start = tojd(1, 2, 2018, 0, 0, 0)     ; starting julian date
  ;jd_start = tojd(12, 10, 2017, 0, 0, 0)   ; starting julian date
  jd_end   = tojd(15, 6, 2018, 0, 0, 0)    ; ending julian date
  
  for jd = jd_start, jd_end do begin
    frmjd, jd, year, mon, day       ; convert to year, month, day and date_dir
    date_dir = string(format='(i4,i2.2,i2.2)', year, mon, fix(day))
    print, date_dir
  
    ; look for flat file for this day
    name = 'V:\CoMP\process\' + date_dir + '\flat.fts'
    r = file_search(name)
    if (r eq '') then begin
      name = 'V:\CoMP\process\' + date_dir + '\level1\flat.fts'
      r = file_search(name)
    endif
  
    if (r ne '') then begin
      info = file_info(r)
      print, info.size
    endif
  
    ; if flat exists, then process it
    if (r ne '' && info.size gt 3072) then begin
      calibrate_comp_wavelength_2, date_dir, lam0, $
                                   offset, h2o, flat_time, off_tell, $
                                   dev=dev
  
      if (flat_time[0] ne 0.0) then begin
        nflat = n_elements(flat_time)
        for i = 0, nflat - 1 do begin
          printf, lun, date_dir, flat_time[i], offset[i, 0], h2o[i, 0], $
                  off_tell[i, 0]
          printf, lun, date_dir, flat_time[i], offset[i, 1], h2o[i, 1], $
                  off_tell[i, 1]
        endfor
      endif
    endif
  
  endfor

  print, systime()

  free_lun, lun
  closepost, dev
end
