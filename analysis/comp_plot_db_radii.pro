; docformat = 'rst'

pro comp_plot_db_radii
  compile_opt strictarr

  db = mgdbmysql()
  db->connect, config_filename='/home/mgalloy/.mysqldb', config_section='mgalloy@databases'
  q = 'select file_name, date_obs, radius1, radius2, occulter_id from comp_eng order by date_obs;'
  results = db->query(q)

  sortby_occulter_id_indices = sort(results.occulter_id)
  unique_occulter_ids_indices = uniq(results.occulter_id, sortby_occulter_id_indices)
  unique_occulter_ids = (results.occulter_id)[unique_occulter_ids_indices]

  n_files = n_elements(results)
  jds = dblarr(n_files)
  for f = 0L, n_files - 1L do begin
    tokens = long(strsplit(results[f].date_obs, ' -:', /extract))
    jds[f] = julday(tokens[1], tokens[2], tokens[0], tokens[3], tokens[4], tokens[5])
  endfor

  epoch1_begin_date = julday(1, 1, 2012)
  epoch1_end_date = julday(12, 7, 2012)

  epoch2_begin_date = julday(5, 1, 2013)
  epoch2_end_date = julday(4, 5, 2018)

  epoch1_radius1_occulter_means   = fltarr(n_elements(unique_occulter_ids))
  epoch1_radius1_occulter_medians = fltarr(n_elements(unique_occulter_ids))
  epoch1_radius1_occulter_stdevs  = fltarr(n_elements(unique_occulter_ids))

  epoch1_radius2_occulter_means   = fltarr(n_elements(unique_occulter_ids))
  epoch1_radius2_occulter_medians = fltarr(n_elements(unique_occulter_ids))
  epoch1_radius2_occulter_stdevs  = fltarr(n_elements(unique_occulter_ids))

  epoch2_radius1_occulter_means   = fltarr(n_elements(unique_occulter_ids))
  epoch2_radius1_occulter_medians = fltarr(n_elements(unique_occulter_ids))
  epoch2_radius1_occulter_stdevs  = fltarr(n_elements(unique_occulter_ids))

  epoch2_radius2_occulter_means   = fltarr(n_elements(unique_occulter_ids))
  epoch2_radius2_occulter_medians = fltarr(n_elements(unique_occulter_ids))
  epoch2_radius2_occulter_stdevs  = fltarr(n_elements(unique_occulter_ids))

  for o = 0L, n_elements(unique_occulter_ids) - 1L do begin
    ind = where(jds ge epoch1_begin_date and jds le epoch1_end_date and results.occulter_id eq unique_occulter_ids[o], /null)

    epoch1_radius1_occulter_means[o] = mean((results.radius1)[ind])
    epoch1_radius1_occulter_medians[o] = median((results.radius1)[ind])
    epoch1_radius1_occulter_stdevs[o] = stdev((results.radius1)[ind])
    print, unique_occulter_ids[o], $
           epoch1_radius1_occulter_means[o], $
           epoch1_radius1_occulter_medians[o], $
           epoch1_radius1_occulter_stdevs[o], $
           format='Epoch 1: Radius 1: Occulter: %d, radius mean: %0.2f, median: %0.2f, stdev: %0.2f'

    epoch1_radius2_occulter_means[o] = mean((results.radius2)[ind])
    epoch1_radius2_occulter_medians[o] = median((results.radius2)[ind])
    epoch1_radius2_occulter_stdevs[o] = stdev((results.radius2)[ind])
    print, unique_occulter_ids[o], $
           epoch1_radius2_occulter_means[o], $
           epoch1_radius2_occulter_medians[o], $
           epoch1_radius2_occulter_stdevs[o], $
           format='Epoch 1: Radius 2: Occulter: %d, radius mean: %0.2f, median: %0.2f, stdev: %0.2f'

    ind = where(jds ge epoch2_begin_date and jds le epoch2_end_date and results.occulter_id eq unique_occulter_ids[o], /null)

    epoch2_radius1_occulter_means[o] = mean((results.radius1)[ind])
    epoch2_radius1_occulter_medians[o] = median((results.radius1)[ind])
    epoch2_radius1_occulter_stdevs[o] = stdev((results.radius1)[ind])
    print, unique_occulter_ids[o], $
           epoch2_radius1_occulter_means[o], $
           epoch2_radius1_occulter_medians[o], $
           epoch2_radius1_occulter_stdevs[o], $
           format='Epoch 2: Radius 1: Occulter: %d, radius mean: %0.2f, median: %0.2f, stdev: %0.2f'

    epoch2_radius2_occulter_means[o] = mean((results.radius2)[ind])
    epoch2_radius2_occulter_medians[o] = median((results.radius2)[ind])
    epoch2_radius2_occulter_stdevs[o] = stdev((results.radius2)[ind])
    print, unique_occulter_ids[o], $
           epoch2_radius2_occulter_means[o], $
           epoch2_radius2_occulter_medians[o], $
           epoch2_radius2_occulter_stdevs[o], $
           format='Epoch 2: Radius 2: Occulter: %d, radius mean: %0.2f, median: %0.2f, stdev: %0.2f'
  endfor

  device, decomposed=1
  !null = label_date(date_format='%Y-%N')
  window, xsize=1200, ysize=900, /free, title='CoMP radii'
  ; plot, jds, results.radius1, /nodata, $
  ;       xstyle=1, xtickformat='label_date', $
  ;       yrange=[210.0, 250.0], ystyle=1
  stride = 10L;1000L
  !p.multi = [0, 1, 2, 0, 0]
  plot, jds[0:*:stride], (results.radius1)[0:*:stride], psym=3, $
        title='Radius 1', $
        xstyle=1, xtickformat='label_date', $
        yrange=[210.0, 250.0], ystyle=1
  plots, dblarr(2) + epoch1_end_date, !y.crange
  plots, dblarr(2) + epoch2_begin_date, !y.crange
  for o = 0L, n_elements(unique_occulter_ids) - 1L do begin
    plots, [epoch1_begin_date, epoch1_end_date], fltarr(2) + epoch1_radius1_occulter_medians[o]
    plots, [epoch2_begin_date, epoch2_end_date], fltarr(2) + epoch2_radius1_occulter_medians[o]
  endfor

  plot, jds[0:*:stride], (results.radius2)[0:*:stride], psym=3, $
        title='Radius 2', $
        xstyle=1, xtickformat='label_date', $
        yrange=[210.0, 250.0], ystyle=1
  plots, dblarr(2) + epoch1_end_date, !y.crange
  plots, dblarr(2) + epoch2_begin_date, !y.crange
  for o = 0L, n_elements(unique_occulter_ids) - 1L do begin
    plots, [epoch1_begin_date, epoch1_end_date], fltarr(2) + epoch1_radius2_occulter_medians[o]
    plots, [epoch2_begin_date, epoch2_end_date], fltarr(2) + epoch2_radius2_occulter_medians[o]
  endfor

  !p.multi = 0
  obj_destroy, db
end


; main-level example program

comp_plot_db_radii

end
