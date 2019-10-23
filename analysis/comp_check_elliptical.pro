; docformat = 'rst'

pro comp_check_elliptical, date, yrange=yrange
  compile_opt strictarr
  @comp_config_common

  n_summary_exts = 3L

  fmt = '(%"%s.comp.flat.fts")'
  flat_filename = filepath(string(date, format=fmt), $
                           subdir=[date, 'level1'], $
                           root=process_basedir)
  fits_open, flat_filename, fcb

  distcorr_radii = fltarr(fcb.nextend - n_summary_exts, 4)
  uncorr_radii = fltarr(fcb.nextend - n_summary_exts, 4)

  for e = 1L, fcb.nextend - n_summary_exts do begin
    fits_read, fcb, data, header, exten_no=e
    distcorr_radii[e - 1L, *] = [sxpar(header, 'ORADIUS1'), $
                                 sxpar(header, 'ORAD1-2'), $
                                 sxpar(header, 'ORADIUS2'), $
                                 sxpar(header, 'ORAD2-2')]

    uncorr_radii[e - 1L, *] = [sxpar(header, 'ORADU1'), $
                               sxpar(header, 'ORADU1-2'), $
                               sxpar(header, 'ORADU2'), $
                               sxpar(header, 'ORADU2-2')]
  endfor
  fits_close, fcb

  xsize = 800
  ysize = 400

  _yrange = n_elements(yrange) eq 0L ? [228.0, 233.0] : yrange

  window, xsize=xsize, ysize=ysize, title='ORADIUS1 (dist corrected)', /free
  plot, distcorr_radii[*, 0], /nodata, $
        xstyle=1, $
        yrange=_yrange, ystyle=1, $
        title='ORADIUS1 (dist corrected)'
  oplot, distcorr_radii[*, 0], color='00ffff'x
  oplot, distcorr_radii[*, 1], color='0000ff'x

  window, xsize=xsize, ysize=ysize, title='ORADIUS2 (dist corrected)', /free
  plot, distcorr_radii[*, 2], /nodata, $
        xstyle=1, $
        yrange=_yrange, ystyle=1, $
        title='ORADIUS2 (dist corrected)'
  oplot, distcorr_radii[*, 2], color='00ffff'x
  oplot, distcorr_radii[*, 3], color='0000ff'x

  window, xsize=xsize, ysize=ysize, title='ORADU1 (not dist corrected)', /free
  plot, uncorr_radii[*, 0], /nodata, $
        xstyle=1, $
        yrange=_yrange, ystyle=1, $
        title='ORADU1 (not dist corrected)'
  oplot, uncorr_radii[*, 0], color='00ffff'x
  oplot, uncorr_radii[*, 1], color='0000ff'x

  window, xsize=xsize, ysize=ysize, title='ORADU2 (not dist corrected)', /free
  plot, uncorr_radii[*, 2], /nodata, $
        xstyle=1, $
        yrange=_yrange, ystyle=1, $
        title='ORADU2 (not dist corrected)'
  oplot, uncorr_radii[*, 2], color='00ffff'x
  oplot, uncorr_radii[*, 3], color='0000ff'x
end


; main-level example program

;date = '20141010'
;yrange = [228.0, 233.0]

date = '20150223'
yrange = [231.0, 235.0]

wave_type = '1074'
config_filename = filepath('comp.elliptic.cfg', $
                           subdir=['..', 'config'], $
                           root=mg_src_root())

comp_initialize, date
comp_configuration, config_filename=config_filename

comp_check_elliptical, date, yrange=yrange

end
