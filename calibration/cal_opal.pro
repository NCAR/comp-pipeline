;+
;  :Name: cal_opal
;
;  :Description: This routine is used to calibrate the brightness of comp diffuser imaages
;  by comparing opal images to images of the solar disk taken through a calibrated neutral
;  density filter.
;  
;  :Author: tomczyk
;-
pro cal_opal
  compile_opt strictarr

  @comp_constants_common
  @comp_config_common
  @comp_mask_constants_common

  ; configure
  date_dir = '20170719'
  config_filename = filepath('comp.transmission.cfg', $
                             subdir=['..', 'config'], $
                             root=mg_src_root())

  comp_initialize, date_dir
  comp_configuration, config_filename=config_filename

  debug = 'no'       ;debug mode ('yes' or 'no')

  opal_file = '100955' ; TODO: need to change to actual opal file
  sun_file = '100955'

  ; opal_file = '130906'         ; opal data
  ; sun_file = '123928'          ; sun data with no occulter and with ND filter
  ; 
  ; opal_file = '130950'         ; opal data
  ; sun_file = '124012'          ; sun data with no occulter and with ND filter
  ; 
  ; opal_file = '131031'         ; opal data
  ; sun_file = '124052'          ; sun data with no occulter and with ND filter
  ; 
  ; opal_file = '131115'         ; opal data
  ; sun_file = '124137'          ; sun data with no occulter and with ND filter

  time1 = total(float(comp_decompose_time(opal_file)) * [1.0, 1.0/60.0, 1.0/3600.0])
  time2 = total(float(comp_decompose_time(sun_file)) * [1.0, 1.0/60.0, 1.0/3600.0])

  time = (time1 + time2) / 2.0   ;find average time

  if (n_elements(raw_basedir) eq 0L) then begin
    raw_basedir = comp_get_route(raw_routing_filename, date_dir, found=found)
  endif

  ; open files
  raw_dir = filepath(date_dir, root=raw_basedir)
  cd, process_basedir

  file = filepath(date_dir + '.' + opal_file + '.FTS', root=raw_dir)
  print, file
  fits_open, file, fcb_opal
  num = fcb_opal.nextend

  file = filepath(date_dir + '.' + sun_file + '.FTS', root=raw_dir)
  print, file
  fits_open, file, fcb_sun


  ; get dark image
  exposure = 250.0
  dark = comp_dark_interp(date_dir, time, exposure)

  ; process images
  window, 0 ,xsize=nx, ysize=nx
  window, 1 ,xsize=nx, ysize=nx

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
    dat = fixrock(dat, 0.030)
    dat = fix_image(dat)
    opal = dat
    opal1 = comp_extract1(opal)
    opal2 = comp_extract2(opal)

    wset, 0
    tvscl, opal1

    fits_read, fcb_sun, dat, header, exten_no=i + 1
    if (sxpar(header, 'DEMULT') eq 0) then dat = demultiplex(dat)
    dat = float(dat) - dark
    dat = fixrock(dat, 0.030)
    dat = fix_image(dat)
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

    r=sqrt((x - x_cent)^2 + (y - y_cent)^2)
    center = where(r lt 40.0)     ; define sun center area
    disk = median(sun1[center])   ; find sun center intensity

    t135 = -3.0 * !pi / 4.0
    t45 = !pi / 4.
    theta = atan(y - y_cent, x - x_cent)
    ann = where((r gt 255. and r lt 275. and theta gt t45-0.2 and theta lt t45+0.2) or $
              (r gt 255. and r lt 275. and theta gt t135-0.2 and theta lt t135+0.2))    ;define annulus
    diffuser = median(opal1[ann])

    if (debug eq 'yes') then print, center, diffuser

    opal_rad[i] = diffuser * 3.e-5 / disk
    print, opal_rad[i]

    opal1[good] = sun1[good]
    opal1[ann] = 0.0
    opal1[center] = 0.0
    wset, 0
    tvscl, opal1
  endfor

  fits_close, fcb_opal
  fits_close, fcb_sun

  print, 'mean:', mean(opal_rad)

  print, 'done'
end