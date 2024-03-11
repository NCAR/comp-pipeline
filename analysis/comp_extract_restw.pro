
pro comp_extract_restw, output_basename_format, method, $
                        program, wave_region, waves, rest_wave, nominal_center, $
                        start_date, end_date
  compile_opt strictarr
  @comp_config_common
  @comp_constants_common

  nx = 620L
  ny = 620L
   c = 299792.458D
  factor = c / nominal_center

  ; set arrays
  peak_intensity = fltarr(nx, ny) 
  doppler_shift   = fltarr(nx, ny)
  velocity           = fltarr(nx, ny)
  line_width       = fltarr(nx, ny)
  wavel              = fltarr(3)
  
  ; define coordinates - this is the proper way to do it
  ; what wss in Steve's code has a column of x-coord equal to zero that was never used
  x = findgen(nx, ny) mod nx - 309.5

  ; FROM MIKE'S UCOMP CODE 
  output_filename = string(method, program, format=output_basename_format)
  openw, lun, output_filename, /get_lun

  date = start_date
  while date ne end_date do begin
    comp_update_configuration, date
    comp_initialize, date

    l2_dir = filepath('', $
                      subdir=[date, 'level2'], $
                      root=process_basedir)
    if (file_test(l2_dir, /directory)) then begin
      basename = string(date, wave_region, program, $
                        format='%s.comp.%s.median.%s.fts.gz')
      filename = filepath(basename, root=l2_dir)
      if (file_test(filename, /regular)) then begin
        fits_open, filename, fcb
        fits_read, fcb, !null, primary_header, exten_no=0

        ;mask the occulter with offsets of +4 pixels and the field with offset of -10 pixels using the occ radius in the header
        mask = comp_l2_mask(primary_header, /image_occulter_radius, occulter_offset=4, field_offset=-10)

        n_wavelengths = sxpar(primary_header, 'NTUNES')
        lambda_zero=sxpar(primary_header, 'WAVE_REF')

        ;set lower maximum for 1079 - to use later
        rest_int_max = 2.0 if lambda_zero gt 1075 then rest_int_max = 1.0
           
        if (n_wavelengths ne 5) then print, 'ERROR: NOT A SYNPOTIC FILE'

        intensity = fltarr(nx, ny, 3)
        for w = 2, 4 do begin
           fits_read, fcb, ext_data, ext_header, exten_no=w
           ;read wavelength
           wavel[w - 2 ]=sxpar( ext_header, 'WAVELENG')
          ; apply geometric mask to each median image before saving it
          intensity[*, *, w - 2] = ext_data * mask
        endfor

        fits_close, fcb

        ; find d_lambda assuming red and blue are symmetric
        d_lambda = wavel[1]-wavel[0]
        
        ; compute gaussian fits for every pixel greater than zero that is not
        ; masked
        good = where_xyz(mask ne 0 $
                           and intensity[*, *, 0] gt 0 $
                           and intensity[*, *, 1] gt 0 $
                           and intensity[*, *, 2] gt 0, $
                         xind=xind, yind=yind, ngood)

        for i = 0, ngood - 1L do begin
          temp_profile = reform(intensity[xind[i], yind[i], *])

          comp_analytic_gauss_fit, temp_profile, d_lambda, doppler, width, peak

          peak_intensity[xind[i], yind[i]] = peak
          doppler_shift[xind[i], yind[i]] = doppler

          ; convert doppler shift to velocity in km/s
          velocity[xind[i], yind[i]] = doppler * factor
          ; convert  W to SIGMA  in km/s          
          line_width[xind[i], yind[i]] = width * factor / sqrt(2)
        endfor

        ; Steve's east/west method - use lower max for 1079
        east0 = where(intensity[*, *, 1] gt rest_int_max  $
                       and intensity[*, *, 1] lt 60 $
                       and x lt 0.0, n_east0)
        west0 = where(intensity[*, *, 1] gt rest_int_max  $
                       and intensity[*, *, 1] lt 60 $
                       and x gt 0.0, $
                     n_west0)

      ;create average east and west profiles    
        if (n_east0 gt 0L && n_west0 gt 0L) then begin
          y = fltarr(3)
          for w = 0, 2 do begin
            temp_intensity = intensity[*, *, w]
            if (method eq 'mean') then begin
              y[w] = 0.5 * (mean(temp_intensity[east0], /nan) + mean(temp_intensity[west0], /nan))
            endif else begin
              y[w] = 0.5 * (median(temp_intensity[east0]) + median(temp_intensity[west0]))
            endelse
          endfor

          ; fit only one gaussian
          ; make sure gaussian fits handles zeros of masked pixels  
          comp_analytic_gauss_fit, y, d_lambda, $
                                   doppler_shift_0, $
                                   line_width_0, $
                                   peak_intensity_0
          ; convert in km/s           
          velocity_0 = doppler_shift_0 * factor
          ; covert W to SIGMA in Km/s           
          line_width_0 = line_width_0 * factor/sqrt(2)
        endif else begin
          doppler_shift_0 = 0.0
          line_width_0 = 0.0
          peak_intensity_0 = 0.0
          velocity_0 =  0.0
        endelse

        ; old COMP method - entire fov - less restrictive criteria in intensity
        all = where(mask gt 0 and velocity ne 0 and abs(velocity) lt 30 $
                      and intensity[*,*,0] gt 0.2 and intensity[*,*,1] gt 1 and intensity[*,*,2] gt 0.2 $
                      and intensity[*,*,0] lt 60 and intensity[*,*,1] lt 60 and intensity[*,*,2] lt 60 $
                      and line_width gt 15.0 and line_width lt 50.0, $  ; this is SIGMA not W - 50km/s correspond to non-thermal velocity of ~70km/s
                       n_all) 

        IF nall gt 0  THEN BEGIN
        if (method eq 'mean') then begin
          doppler_shift_1  = mean(doppler_shift[all], /nan)
          velocity_1       = mean(velocity[all], /nan)
          line_width_1     = mean(line_width[all], /nan)
          peak_intensity_1 = mean(peak_intensity[all], /nan)
        endif else begin
          doppler_shift_1  = median(doppler_shift[all])
          velocity_1       = median(velocity[all])
          line_width_1     = median(line_width[all])
          peak_intensity_1 = median(peak_intensity[all])
       endelse
      ENDIF  ELSE BEGIN
         doppler_shift_1  = 0.0
          velocity_1       = 0.0
          line_width_1     = 0.0
          peak_intensity_1 = 0.0
       ENDELSE

        ; east/west COMP method - less restrictive criteria in intensity 
        east2 = where(mask gt 0 and velocity ne 0 and velocity lt 20 and velocity gt -30 and x lt 0.0  $
                       and intensity[*,*,0] gt 0.2 and intensity[*,*,1] gt 1 and intensity[*,*,2] gt 0.2 $
                       and intensity[*,*,0] lt 60   and intensity[*,*,1] lt 60 and intensity[*,*,2] lt 60 $
                       and line_width gt 15.0 and line_width lt 50.0, $
                     n_east2)
                       
        west2 = where(mask gt 0 and velocity ne 0 and velocity lt 20 and velocity gt -30 and x gt 0.0  $
                       and intensity[*,*,0] gt 0.2 and intensity[*,*,1] gt 1 and intensity[*,*,2] gt 0.2 $
                       and intensity[*,*,0] lt 60   and intensity[*,*,1] lt 60 and intensity[*,*,2] lt 60 $
                       and line_width gt 15.0 and line_width lt 50.0, $
                     n_west2)

        IF (n_east2 gt 0 and n_west2 gt 0 ) THEN BEGIN
        ; the east and west velocity should never be NaN, but if it is NaN we want to carry it into the mean rest wavelength 
        if (method eq 'mean') then begin
          doppler_shift_2  = mean( [mean([doppler_shift[east2]], /nan), mean([doppler_shift[west2]], /nan)] )
          velocity_2       = mean([mean([velocity[east2]], /nan), mean([velocity[west2]], /nan)] )
          line_width_2     = mean([mean([line_width[east2]], /nan), mean([line_width[west2]], /nan)] )
          peak_intensity_2 = mean([mean([peak_intensity[east2]], /nan),  mean([peak_intensity[west2]], /nan)] )
        endif else begin
          doppler_shift_2  = mean([median([doppler_shift[east2]]), median([doppler_shift[west2]])] )
          velocity_2         = mean([median([velocity[east2]]), median([velocity[west2]])] )
          line_width_2     = mean([median([line_width[east2]]), median([line_width[west2]])] )
          peak_intensity_2 = mean([median([peak_intensity[east2]]),  median([peak_intensity[west2]])] )
        endelse
       ENDIF  ELSE BEGIN
         doppler_shift_2  = 0.0
          velocity_2       = 0.0
          line_width_2     = 0.0
          peak_intensity_2 = 0.0
       ENDELSE

       
        ; east/west COMP method 2 - more restrictive criteria in intensity 
        east22 = where(mask gt 0 and velocity ne 0 and velocity gt -30 and velocity lt 20 and x lt 0.0  $
                        and intensity[*,*,0] gt 0.5 and intensity[*,*,1] gt rest_int_max and intensity[*,*,2] gt 0.5 $
                        and intensity[*,*,0] lt 60 and intensity[*,*,1] lt 60 and intensity[*,*,2] lt 60 $
                        and line_width gt 15.0 and line_width lt 50.0, $
                      n_east22)

        west22 = where(mask gt 0 and velocity ne 0 and  velocity gt -30 and velocity lt 20 and x gt 0.0  $
                        and intensity[*,*,0] gt 0.5 and intensity[*,*,1] gt rest_int_max and intensity[*,*,2] gt 0.5 $
                        and intensity[*,*,0] lt 60 and intensity[*,*,1] lt 60 and intensity[*,*,2] lt 60 $
                        and line_width gt 15.0 and line_width lt 50.0, $
                      n_west22)

         IF (n_east22 gt 0 and n_west22 gt 0 ) THEN BEGIN
        if (method eq 'mean') then begin
          doppler_shift_22  = mean([mean([doppler_shift[east22]], /nan), mean([doppler_shift[west22]], /nan)] )
          velocity_22       = mean([mean([velocity[east22]], /nan), mean([velocity[west22]], /nan)] )
          line_width_22     = mean([mean([line_width[east22]], /nan), mean([line_width[west22]], /nan)] )
          peak_intensity_22 = mean([mean([peak_intensity[east22]], /nan), mean([peak_intensity[west22]], /nan)] )
        endif else begin
          doppler_shift_22  = mean([median([doppler_shift[east22]]), median([doppler_shift[west22]])] )
          velocity_22       = mean([median([velocity[east22]]), median([velocity[west22]])] )
          line_width_22     = mean([median([line_width[east22]]), median([line_width[west22]])] )
          peak_intensity_22 = mean([median([peak_intensity[east22]]), median([peak_intensity[west22]])] )
        endelse
       ENDIF  ELSE BEGIN
         doppler_shift_22  = 0.0
          velocity_22      = 0.0
          line_width_22     = 0.0
          peak_intensity_22 = 0.0
       ENDELSE
       
        ; east/west COMP method but in detector frame

        ; rotate back image to detector frame
        p_angle = sxpar(primary_header, 'SOLAR_P0')

        device_mask = rot(mask, -p_angle)
        dev_bad = where(device_mask lt 0.5, nbad)
        if (nbad gt 0) then device_mask[dev_bad] = 0

        ; use device_mask to zeros bad pixels
        device_velocity = rot(velocity, - p_angle)
        device_velocity *= device_mask

        device_line_width = rot(line_width, - p_angle)
        device_line_width *= device_mask

        device_doppler_shift = rot(doppler_shift, -p_angle)
        device_doppler_shift *= device_mask

        device_peak_intensity = rot(peak_intensity, -p_angle)
        device_peak_intensity *= device_mask

        device_intensity = fltarr(nx, ny, 3)
        for w = 0, 2 do begin
          device_intensity[*, *, w] = rot(intensity[*,*,w], - p_angle)
          device_intensity[*, *, w] *= device_mask
        endfor

        ; more restrictive criteria in intensity but in detector frame
        device_east3 = where(device_mask gt 0 and device_velocity ne 0 and velocity lt 20 and velocity gt -30 and x lt 0.0  $
                              and device_intensity[*,*,0] gt 0.5 and device_intensity[*,*,1] gt rest_int_max and device_intensity[*,*,2] gt 0.5 $
                              and device_intensity[*,*,0] lt 60 and device_intensity[*,*,1] lt 50 and device_intensity[*,*,2] lt 60 $
                              and device_line_width gt 15.0 and device_line_width lt 50.0, $
                            n_east3)

        device_west3 = where(device_mask gt 0 and device_velocity ne 0 and velocity lt 20 and velocity gt -30  and x gt 0.0  $
                              and device_intensity[*,*,0] gt 0.5 and device_intensity[*,*,1] gt rest_int_max and device_intensity[*,*,2] gt 0.5 $
                              and device_intensity[*,*,0] lt 60 and device_intensity[*,*,1] lt 60 and device_intensity[*,*,2] lt 60 $
                              and device_line_width gt 15.0 and device_line_width lt 50.0, $
                            n_west3)

        IF (n_east3 gt 0 and n_west3 gt 0 ) THEN BEGIN
        if (method eq 'mean') then begin
          doppler_shift_3  = mean([mean([device_doppler_shift[device_east3]], /nan),  mean([device_doppler_shift[device_west3]], /nan)] )
          velocity_3       = mean([mean([device_velocity[device_east3]], /nan),  mean([device_velocity[device_west3]], /nan)] )
          line_width_3     = mean([mean([device_line_width[device_east3]], /nan), mean([device_line_width[device_west3]], /nan)] )
          peak_intensity_3 = mean([mean([device_peak_intensity[device_east3]], /nan), mean([device_peak_intensity[device_west3]], /nan)] )
        endif else begin
          doppler_shift_3  = mean([median([device_doppler_shift[device_east3]]),  median([device_doppler_shift[device_west3]])] )
          velocity_3       = mean([median([device_velocity[device_east3]]),  median([device_velocity[device_west3]])] )
          line_width_3     = mean([median([device_line_width[device_east3]]), median([device_line_width[device_west3]])] )
          peak_intensity_3 = mean([median([device_peak_intensity[device_east3]]), median([device_peak_intensity[device_west3]])] )
       endelse
        ENDIF  ELSE BEGIN
         doppler_shift_3  = 0.0
          velocity_3       = 0.0
          line_width_3     = 0.0
          peak_intensity_3 = 0.0
       ENDELSE

        ; TODO
        ; MUST SAVE THE NUMBER OF POINTS n_east0, n_west0, n_all, n_east2, n_west2, n_east22, n_west22, n_east3, n_west3      
        printf, lun, date, $
          doppler_shift_0, velocity_0, line_width_0, peak_intensity_0, $    ;  n_east0, n_west0
          doppler_shift_1, velocity_1, line_width_1, peak_intensity_1, $    ; n_all 
          doppler_shift_2, velocity_2, line_width_2, peak_intensity_2, $    ; n_east2, n_west2
          doppler_shift_22, velocity_22, line_width_22, peak_intensity_22, $  ; n_east22, n_west22
          doppler_shift_3, velocity_3, line_width_3, peak_intensity_3, $       ; n_east3, n_west3
          format='(A, 20(", ", F0.5))'
        print, date, $
          doppler_shift_0, velocity_0, line_width_0, peak_intensity_0, $
          doppler_shift_1, velocity_1, line_width_1, peak_intensity_1, $
          doppler_shift_2, velocity_2, line_width_2, peak_intensity_2, $
          doppler_shift_22, velocity_22, line_width_22, peak_intensity_22, $
          doppler_shift_3, velocity_3, line_width_3, peak_intensity_3, $
          format='(A, 20(", ", F0.5))'
      endif else begin
        print, filename, format='file does not exist: %s'
      endelse
    endif else begin
      print, l2_dir, format='l2_dir does not exist: %s'
    endelse

    next:
    date = ucomp_increment_date(date)
  endwhile
 
  free_lun, lun
end


; main-level example program

config_basename = 'comp.reprocess-201204.cfg'
config_filename = filepath(config_basename, $
                           subdir=['..', 'config'], $
                           root=mg_src_root())
comp_configuration, config_filename=config_filename

program = 'synoptic'

; set constants
wave_region = '1074'    ; repeat for '1079'
waves = [1074.50D, 1074.62D, 1074.74D]
rest_wave = 1074.62D     ; change for 1079
nominal_center = 1074.70D ; change for 1079

start_date = '20121201'
end_date = '20180407'

method = 'mean'
output_basename_format = 'comp.restwvl.%s.%s.txt'

comp_extract_restw, output_basename_format, method, $
                    program, wave_region, waves, rest_wave, nominal_center, $
                    start_date, end_date

end
