; docformat = 'rst'

pro comp_plot_radii, radii_db, start_date=start_date, end_date=end_date, yrange=yrange
  compile_opt strictarr

  types = ['date', 'float', 'string', 'float', 'long', $   ;'string', $
           'float', 'float', 'float', 'float', $
           'float', 'float', 'float', 'float']
  data = read_csv(radii_db, types=types)

  dates = data.field01
  times = data.field02
  wave_types = data.field03
  wavelengths = data.field04
  beams = data.field05
  ;occulters = data.field06

  n_rows = n_elements(dates)

  distcorr_radii = fltarr(n_rows, 4)
  distcorr_radii[*, 0] = data.field06
  distcorr_radii[*, 1] = data.field07
  distcorr_radii[*, 2] = data.field08
  distcorr_radii[*, 3] = data.field09

  uncorr_radii = fltarr(n_rows, 4)
  uncorr_radii[*, 0] = data.field10
  uncorr_radii[*, 1] = data.field11
  uncorr_radii[*, 2] = data.field12
  uncorr_radii[*, 3] = data.field13

  ymd = long(comp_decompose_date(dates))
  jds = julday(ymd[*, 1], ymd[*, 2], ymd[*, 0], times)

  if (n_elements(start_date) eq 0L) then begin
    start_dt = jds[0]
  endif else begin
    start_ymd = comp_decompose_date(start_date)
    start_dt = julday(start_ymd[1], start_ymd[2], start_ymd[0], 0.0)
  endelse

  if (n_elements(end_date) eq 0L) then begin
    end_dt = jds[-1]
  endif else begin
    end_ymd = comp_decompose_date(end_date)
    end_dt = julday(end_ymd[1], end_ymd[2], end_ymd[0], 24.0)
  endelse

  good_indices = where(jds ge start_dt and jds le end_dt, n_good_indices)
  if (n_good_indices eq 0L) then message, 'no data in date range'

  xsize = 800
  ysize = 400

  psym = -1

  ;occulter_change = [0B, occulters[0:-2] ne occulters[1:-1]]
  ;occulter_change_indices = where(occulter_change, n_occulter_changes)

  minimum_value = min([min(distcorr_radii), min(uncorr_radii)])
  maximum_value = max([max(distcorr_radii), max(uncorr_radii)])

  _yrange = n_elements(yrange) eq 0L ? [minimum_value, maximum_value] : yrange
  date_range_fmt = '(C(CYI4.4, "-", CMOI2.2, "-", CDI2.2), " to ", C(CYI4.4, "-", CMOI2.2, "-", CDI2.2))'
  date = string(start_dt, end_dt, format=date_range_fmt)
  !null = label_date(date_format='%Y-%N-%D')

  window, xsize=xsize, ysize=2 * ysize, title=date, /free
  !p.multi = [0, 1, 2]

;  plot, jds[good_indices], uncorr_radii[good_indices, 0], /nodata, $
;        xstyle=1, xtickformat='label_date', xticks=8, $
;        yrange=_yrange, ystyle=1, $
;        title=string(date, wave_types[0], $
;                     format='(%"%s [wave type: %s] - ORADU1 (not dist corrected)")')
;  oplot, jds[good_indices], uncorr_radii[good_indices, 0], color='00ffff'x, psym=psym
;  oplot, jds[good_indices], uncorr_radii[good_indices, 1], color='0000ff'x, psym=psym
  ;for o = 0L, n_occulter_changes - 1L do begin
  ;  oplot, dblarr(2) + jds[occulter_change_indices[o]], !y.crange, $
  ;         color='00ff00'x
  ;endfor

  plot, jds[good_indices], distcorr_radii[good_indices, 0], /nodata, $
        xstyle=1, xtickformat='label_date', xticks=8, $
        yrange=_yrange, ystyle=1, $
        title=string(date, wave_types[0], $
                     format='(%"%s [wave_type: %s] - ORADIUS1 (dist corrected)")')
  oplot, jds[good_indices], distcorr_radii[good_indices, 0], color='00ffff'x, psym=psym
  oplot, jds[good_indices], distcorr_radii[good_indices, 1], color='0000ff'x, psym=psym
  ;for o = 0L, n_occulter_changes - 1L do begin
  ;  oplot, dblarr(2) + jds[occulter_change_indices[o]], !y.crange, $
  ;         color='00ff00'x
  ;endfor

  xyouts, 0.1, 0.94, /normal, 'width', color='00ffff'x
  xyouts, 0.1, 0.92, /normal, 'height', color='0000ff'x

;  plot, jds[good_indices], uncorr_radii[good_indices, 2], /nodata, $
;        xstyle=1, xtickformat='label_date', xticks=8, $
;        yrange=_yrange, ystyle=1, $
;        title=string(date, wave_types[0], $
;                     format='(%"%s [wave type: %s] - ORADU2 (not dist corrected)")')
;  oplot, jds[good_indices], uncorr_radii[good_indices, 2], color='00ffff'x, psym=psym
;  oplot, jds[good_indices], uncorr_radii[good_indices, 3], color='0000ff'x, psym=psym
  ;for o = 0L, n_occulter_changes - 1L do begin
  ;  oplot, dblarr(2) + jds[occulter_change_indices[o]], !y.crange, $
  ;         color='00ff00'x
  ;endfor

  plot, jds[good_indices], distcorr_radii[good_indices, 2], /nodata, $
        xstyle=1, xtickformat='label_date', xticks=8, $
        yrange=_yrange, ystyle=1, $
        title=string(date, wave_types[0], $
                     format='(%"%s [wave type: %s] - ORADIUS2 (dist corrected)")')
  oplot, jds[good_indices], distcorr_radii[good_indices, 2], color='00ffff'x, psym=psym
  oplot, jds[good_indices], distcorr_radii[good_indices, 3], color='0000ff'x, psym=psym
  ;for o = 0L, n_occulter_changes - 1L do begin
  ;  oplot, dblarr(2) + jds[occulter_change_indices[o]], !y.crange, $
  ;         color='00ff00'x
  ;endfor

  window, xsize=2*xsize, ysize=2 * ysize, title=date, /free
  !p.multi = [0, 2, 2]

  nbins = 100

  h = histogram(distcorr_radii[good_indices, 0], nbins=nbins, locations=locs)
  mg_histplot, locs, h, color='00ffff'x, /fill, $
               title=string(date, wave_types[0], $
                            format='(%"Width: %s [wave type: %s] - ORADIUS1 (dist corrected)")')

  h = histogram(distcorr_radii[good_indices, 1], nbins=nbins, locations=locs)
  mg_histplot, locs, h, color='0000ff'x, /fill, $
               title=string(date, wave_types[0], $
                            format='(%"Height: %s [wave type: %s] - ORADIUS1 (dist corrected)")')

  h = histogram(distcorr_radii[good_indices, 2], nbins=nbins, locations=locs)
  mg_histplot, locs, h, color='00ffff'x, /fill, $
               title=string(date, wave_types[0], $
                            format='(%"Width: %s [wave type: %s] - ORADIUS2 (dist corrected)")')

  h = histogram(distcorr_radii[good_indices, 3], nbins=nbins, locations=locs)
  mg_histplot, locs, h, color='0000ff'x, /fill, $
               title=string(date, wave_types[0], $
                            format='(%"Height: %s [wave type: %s] - ORADIUS2 (dist corrected)")')

  !p.multi = 0
end


; main-level example program

; winter occulter
;yrange = [229.0, 234.5]
;start_date = '20170101'
;end_date = '20170317'

; spring/fall occulter
;yrange = [228.0, 231.5]
;start_date = '20170317'
;end_date = '20170607'

; summer occulter
;yrange = [225.0, 228.5]
;start_date = '20170607'
;end_date = '20170910'

; spring/fall occulter
;yrange = [228.0, 231.5]
;start_date = '20170911'
;end_date = '20171030'

; winter occulter
yrange = [231.5, 234.5]
start_date = '20171031'
end_date = '20171231'

comp_plot_radii, 'comp-2017-radii.csv', $
                 start_date=start_date, end_date=end_date, $
                 yrange=yrange

end
