;+
; This routine is used to calibrate the brightness of comp diffuser images
; by comparing opal images to images of the solar disk taken through a
; calibrated neutral density filter.
;
; The "diffuser" OBS_PLAN images contain the required data for this
; calibration. To identify the files check the primary header for the
; following FITS keywords:
;
;   - COVER=1, OPAL=0, and OBS_PLAN=diffuser => special ND
;   - COVER=0, OPAL=1, and OBS_PLAN=diffuser => flat
;   - COVER=1, OPAL=1, and OBS_PLAN=diffuser => dark
;
;  :Author: tomczyk, mgalloy
;-
pro cal_opal
  compile_opt strictarr

  @comp_constants_common
  @comp_config_common
  @comp_mask_constants_common

  ; configure

  ; date = '20170721'

  ; 21.38
  ; opal_file = '104040'
  ; sun_file = '104901'

  ; 21.55
  ; opal_file = '104124'
  ; sun_file = '104945'

  ; 22.00
  ; opal_file = '104248'
  ; sun_file = '105025'

  ; 21.62
  ; opal_file = '104248'
  ; sun_file = '105109'

  ; 20170721.103938.FTS  dark   dark_250ms
  ; 20170721.104040.FTS  flat   1074_QU_11_250ms_2be  89 exts
  ; 20170721.104124.FTS  flat   1074_V_5_250ms_2beam  81 exts
  ; 20170721.104204.FTS  flat   1074_QU_11_250ms_2be  89 exts
  ; 20170721.104248.FTS  flat   1074_V_5_250ms_2beam  81 exts
  ; 20170721.104811.FTS  flat   dark_250ms
  ; 20170721.104901.FTS  ND     1074_QU_11_250ms_2be  89 exts
  ; 20170721.104945.FTS  ND     1074_V_5_250ms_2beam  81 exts
  ; 20170721.105025.FTS  ND     1074_QU_11_250ms_2be  89 exts
  ; 20170721.105109.FTS  ND     1074_V_5_250ms_2beam  81 exts
  ; 20170721.105536.FTS  dark   dark_250ms
  ; 20170721.105638.FTS  flat   1074_QU_11_250ms_2be  89 exts
  ; 20170721.105722.FTS  flat   1074_V_5_250ms_2beam  81 exts
  ; 20170721.105802.FTS  flat   1074_QU_11_250ms_2be  89 exts
  ; 20170721.105846.FTS  flat   1074_V_5_250ms_2beam  81 exts


  date = '20180404'

  ; 22.82
  ; opal_file = '084025'
  ; sun_file = '084758'

  ; 22.63
  ; opal_file = '084109'
  ; sun_file = '084842'

  ; 22.59
  ; opal_file = '084149'
  ; sun_file = '084922'

  ; 22.61
  opal_file = '084233'
  sun_file = '085006'

  ; 20180404.083636.FTS  dark  dark_250ms
  ; 20180404.084025.FTS  flat  1074_QU_11_250ms_2be  89 exts
  ; 20180404.084109.FTS  flat  1074_V_5_250ms_2beam  81 exts
  ; 20180404.084149.FTS  flat  1074_QU_11_250ms_2be  89 exts
  ; 20180404.084233.FTS  flat  1074_V_5_250ms_2beam  81 exts
  ; 20180404.084709.FTS  dark  dark_250ms
  ; 20180404.084758.FTS  ND    1074_QU_11_250ms_2be  89 exts
  ; 20180404.084842.FTS  ND    1074_V_5_250ms_2beam  81 exts
  ; 20180404.084922.FTS  ND    1074_QU_11_250ms_2be  89 exts
  ; 20180404.085006.FTS  ND    1074_V_5_250ms_2beam  81 exts
  ; 20180404.085839.FTS  dark  dark_250ms
  ; 20180404.085928.FTS  flat  1074_QU_11_250ms_2be  89 exts
  ; 20180404.090012.FTS  flat  1074_V_5_250ms_2beam  81 exts
  ; 20180404.090052.FTS  flat  1074_QU_11_250ms_2be  89 exts
  ; 20180404.090155.FTS  flat  1074_V_5_250ms_2beam  81 exts



  ; date = '20161214'

  ; 25.72 (Steve selected)
  ; opal_file = '130906'         ; opal data
  ; sun_file = '123928'          ; sun data with no occulter and with ND filter

  ; 25.72 (Steve selected)
  ; opal_file = '130950'         ; opal data
  ; sun_file = '124012'          ; sun data with no occulter and with ND filter

  ; 25.74 (Steve selected)
  ; opal_file = '131031'         ; opal data
  ; sun_file = '124052'          ; sun data with no occulter and with ND filter

  ; 25.70 (Steve selected)
  ; opal_file = '131115'         ; opal data
  ; sun_file = '124137'          ; sun data with no occulter and with ND filter

  config_filename = filepath('comp.transmission.cfg', $
                             subdir=['..', 'config'], $
                             root=mg_src_root())

  comp_initialize, date
  comp_configuration, config_filename=config_filename

  debug = 'no'       ;debug mode ('yes' or 'no')
  nd_transmission = 3.0e-5

  time1 = total(float(comp_decompose_time(opal_file)) * [1.0, 1.0/60.0, 1.0/3600.0])
  time2 = total(float(comp_decompose_time(sun_file)) * [1.0, 1.0/60.0, 1.0/3600.0])

  time = (time1 + time2) / 2.0   ;find average time

  if (n_elements(raw_basedir) eq 0L) then begin
    raw_basedir = comp_get_route(raw_routing_filename, date, "raw", found=found)
  endif

  ; open files
  raw_dir = filepath(date, root=raw_basedir)
  ; raw_dir = filepath('bad', subdir=date, root=raw_basedir)
  cd, process_basedir

  file = filepath(date + '.' + opal_file + '.FTS', root=raw_dir)
  print, file, format='(%"opal filename: %s")'
  fits_open, file, fcb_opal
  num = fcb_opal.nextend

  file = filepath(date + '.' + sun_file + '.FTS', root=raw_dir)
  print, file, format='(%"sun filename: %s")'
  fits_open, file, fcb_sun


  ; get dark image
  exposure = 250.0
  dark = comp_dark_interp(date, time, exposure)

  ; process images
  window, 0, xsize=nx, ysize=nx
  window, 1, xsize=nx, ysize=nx

  x = rebin(findgen(nx),nx,nx)
  y = transpose(x)
  x0 = 291    ;  approximate center of sub-image 1
  y0 = 324
  r = sqrt((x - x0)^2 + (y - y0)^2)

  opal_rad = fltarr(num)     ; array to hold diffuser radiance values

  for i = 0, num - 1 do begin
    fits_read, fcb_opal, dat, header, exten_no=i + 1
    if (sxpar(header, 'DEMULT') eq 0) then dat = demultiplex(dat)
    dat = float(dat) -dark
    dat = comp_fixrock(dat, 0.030)
    dat = comp_fix_image(dat)
    opal = dat
    opal1 = comp_extract1(opal)
    opal2 = comp_extract2(opal)

    wset, 0
    tvscl, opal1

    fits_read, fcb_sun, dat, header, exten_no=i + 1
    if (sxpar(header, 'DEMULT') eq 0) then dat = demultiplex(dat)
    dat = float(dat) - dark
    dat = comp_fixrock(dat, 0.030)
    dat = comp_fix_image(dat)
    sun = dat
    sun1 = comp_extract1(sun)
    sun2 = comp_extract2(sun)

    wset, 1
    tvscl, sun1

    good = where(r lt 240.0 and sun1 gt max(sun1) / 4.0)
  
    disk = fltarr(nx, nx)    ;compute actual sun center
    disk[good] = 1.0
    x_cent = total(x * disk) / total(disk)
    y_cent = total(y * disk) / total(disk)
    if debug eq 'yes' then print, x_cent, y_cent

    r = sqrt((x - x_cent)^2 + (y - y_cent)^2)
    center = where(r lt 40.0)     ; define sun center area
    disk = median(sun1[center])   ; find sun center intensity

    t135 = -3.0 * !pi / 4.0
    t45 = !pi / 4.0
    theta = atan(y - y_cent, x - x_cent)
    ann = where((r gt 255. and r lt 275. and theta gt t45-0.2 and theta lt t45+0.2) or $
              (r gt 255. and r lt 275. and theta gt t135-0.2 and theta lt t135+0.2), count)    ;define annulus
    diffuser = count gt 0L ? median(opal1[ann]) : !values.f_nan

    if (debug eq 'yes') then print, center, diffuser
    opal_rad[i] = diffuser * nd_transmission / disk
    if (opal_rad[i] lt 0.0) then opal_rad[i] = !values.f_nan
    print, i, opal_rad[i] * 1e6, format='(%"[ext %03d] diffuser radiance: %0.2f")'

    opal1[good] = sun1[good]
    opal1[ann] = 0.0
    opal1[center] = 0.0
    wset, 0
    tvscl, opal1
  endfor

  fits_close, fcb_opal
  fits_close, fcb_sun

  mean_opal_rad = mean(opal_rad, /nan)
  print, mean_opal_rad * 1e6, format='(%"mean: %0.2f")'

  print, 'done'
end