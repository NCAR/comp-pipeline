; docformat = 'rst'

pro comp_check_elliptical, process_basedir, date, wave_type, yrange=yrange
  compile_opt strictarr
;  @comp_config_common
  @comp_constants_common

  n_summary_exts = 3L

  fmt = '(%"%s.comp.flat.fts")'
  flat_filename = filepath(string(date, format=fmt), $
                           subdir=[date + '.original', 'level1'], $
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


  fits_read, fcb, times, times_header, exten_no=fcb.nextend - 2
  fits_read, fcb, wavelengths, wavelengths_header, exten_no=fcb.nextend - 1
  fits_close, fcb

  if (n_elements(wave_type) gt 0L) then begin
    regions = [center1074, center1079, center1083]
    wave_names = ['1074', '1079', '1083']

    expanded_wavelengths = rebin(reform(wavelengths, 1, n_elements(wavelengths)), $
                                 n_elements(regions), $
                                 n_elements(wavelengths))
    regions = rebin(reform(regions, n_elements(regions), 1), $
                    n_elements(regions), $
                    n_elements(wavelengths))
    diffs = abs(expanded_wavelengths - regions)
    !null = min(diffs, dimension=1, closest_indices)

    indices = where(wave_names[closest_indices mod 3] eq wave_type, count)
    if (count eq 0L) then begin
      print, wave_type, format='(%"no extensions of wave_type %s")'
      return
    endif

    distcorr_radii = distcorr_radii[indices, *]
    uncorr_radii = uncorr_radii[indices, *]
    times = times[indices]

    wave_name = string(wave_type, format='(%"%s nm")')
  endif else begin
    wave_name = 'all'
  endelse

  xsize = 800
  ysize = 400

  psym = -1

  _yrange = n_elements(yrange) eq 0L ? [228.0, 233.0] : yrange

  window, xsize=2 * xsize, ysize=2 * ysize, title=date, /free
  !p.multi = [0, 2, 2]

  plot, times, uncorr_radii[*, 0], /nodata, $
        xstyle=1, $
        yrange=_yrange, ystyle=1, $
        title=string(date, wave_name, $
                     format='(%"%s [wave type: %s] - ORADU1 (not dist corrected)")')
  oplot, times, uncorr_radii[*, 0], color='00ffff'x, psym=psym
  oplot, times, uncorr_radii[*, 1], color='0000ff'x, psym=psym

  xyouts, 0.05, 0.94, /normal, 'width', color='00ffff'x
  xyouts, 0.05, 0.92, /normal, 'height', color='0000ff'x

  plot, times, distcorr_radii[*, 0], /nodata, $
        xstyle=1, $
        yrange=_yrange, ystyle=1, $
        title=string(date, wave_name, $
                     format='(%"%s [wave_type: %s] - ORADIUS1 (dist corrected)")')
  oplot, times, distcorr_radii[*, 0], color='00ffff'x, psym=psym
  oplot, times, distcorr_radii[*, 1], color='0000ff'x, psym=psym

  plot, times, uncorr_radii[*, 2], /nodata, $
        xstyle=1, $
        yrange=_yrange, ystyle=1, $
        title=string(date, wave_name, $
                     format='(%"%s [wave type: %s] - ORADU2 (not dist corrected)")')
  oplot, times, uncorr_radii[*, 2], color='00ffff'x, psym=psym
  oplot, times, uncorr_radii[*, 3], color='0000ff'x, psym=psym

  plot, times, distcorr_radii[*, 2], /nodata, $
        xstyle=1, $
        yrange=_yrange, ystyle=1, $
        title=string(date, wave_name, $
                     format='(%"%s [wave type: %s] - ORADIUS2 (dist corrected)")')
  oplot, times, distcorr_radii[*, 2], color='00ffff'x, psym=psym
  oplot, times, distcorr_radii[*, 3], color='0000ff'x, psym=psym

  !p.multi = 0
end


; main-level example program

; TODO:
;   - only wavetype 1074
;   - record (both dist and uncorr) radius x/y, time, date, occulter, wavelength, beam
;   - annotate radius1/2, red vs yellow

;date = '20141010'
;yrange = [228.0, 233.0]

date = '20150223'
yrange = [231.0, 235.0]

; bad radius2?
;date = '20170621'

;date = '20170213'
;yrange = [220.0, 240.0]

wave_type = '1074'
;wave_type = '1079'

config_filename = filepath('comp.elliptic.cfg', $
                           subdir=['..', 'config'], $
                           root=mg_src_root())

comp_initialize, date
comp_configuration, config_filename=config_filename

process_basedir = '/hao/sunset/Data/CoMP/process.elliptic'
comp_check_elliptical, process_basedir, date, wave_type, yrange=yrange

end
