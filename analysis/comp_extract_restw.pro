
pro comp_extract_restw, output_basename_format, method, $
                        program, wave_region, waves, rest_wave, nominal_center, $
                        start_date, end_date
  compile_opt strictarr
  @comp_config_common
  @comp_constants_common

  nx = 620L
  ny = 620L
  d_lambda = 0.12D
  c = 299792.458D
  factor = c / nominal_center

  ; set arrays
  peak_intensity = fltarr(nx, ny) 
  doppler_shift  = fltarr(nx, ny)
  velocity       = fltarr(nx, ny)
  line_width     = fltarr(nx, ny)

  ; define coordinates - this is the proper way to do it
  ; what is in Steve's code has a column of x-coord equal to zero that is never
  ; used
  x = findgen(nx, ny) mod nx - 309.5

  ; FROM MIKE'S UCOMP CODE - MODIFIED TO WORK ON COMP DIRECTORIES -- PLEASE
  ; CHECK IT
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

        ; make mask for median intensity to use later - the mask should include
        ; masking the post - Mike please check this
        ; should mask the occulter with offsets of +3 pixels and the field with
        ; offset of -10 pixels 
        mask = comp_l2_mask(primary_header)

        n_wavelengths = sxpar(primary_header, 'NTUNES')

        if (n_wavelengths ne 5) then print, 'ERROR: NOT A SYNPOTIC FILE'

        intensity = fltarr(nx, ny, 3)
        for w = 2, 4 do begin
          fits_read, fcb, ext_data, ext_header, exten_no=w

          ; apply geometric mask to each median image before saving it
          intensity[*, *, w - 2] = ext_data * mask
        endfor

        fits_close, fcb

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
          ; convert  W to sigma  in km/s          
          line_width[xind[i], yind[i]] = width * factor / sqrt(2)
        endfor

        ; Steve's east/west method 
        east = where(intensity[*, *, 1] gt 2 $
                       and intensity[*, *, 1] lt 60 $
                       and x lt 0.0, n_east)
        west = where(intensity[*, *, 1] gt 2 $
                       and intensity[*, *, 1] lt 60 $
                       and x gt 0.0, $
                     n_west)

        if (n_east gt 0L && n_west gt 0L) then begin
          y = fltarr(3)
          for w = 0, 2 do begin
            temp_intensity = intensity[*, *, w]
            if (method eq 'mean') then begin
              y[w] = 0.5 * (mean(temp_intensity[east], /nan) + mean(temp_intensity[west], /nan))
            endif else begin
              y[w] = 0.5 * (median(temp_intensity[east]) + median(temp_intensity[west]))
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
          ; covert W to sigma in Km/s           
          line_width_0 = line_width_0 * factor/sqrt(2)
        endif else begin
          doppler_shift_0 = 0.0
          line_width_0 = 0.0
          peak_intensity_0 = 0.0
          velocity_0 =  0.0
        endelse

        ; old COMP method - entire fov 
        all = where(mask gt 0 and velocity ne 0 and abs(velocity) lt 30 $
                      and intensity[*,*,0] gt 0.2 and intensity[*,*,1] gt 1 and intensity[*,*,2] gt 0.2 $
                      and intensity[*,*,0] lt 60 and intensity[*,*,1] lt 60 and intensity[*,*,2] lt 60 $
                      and line_width gt 15.0 and line_width lt 50.0, $
                    n_all) ; this is sigma not W - 50km/s correspond to non-thermal velocity of ~70km/s

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

        ; east/west COMP method - low intensity thresholds
        east = where(mask gt 0 and velocity ne 0 and abs(velocity) lt 30 and x lt 0.0  $
                       and intensity[*,*,0] gt 0.2 and intensity[*,*,1] gt 1 and intensity[*,*,2] gt 0.2 $
                       and intensity[*,*,0] lt 60   and intensity[*,*,1] lt 60 and intensity[*,*,2] lt 60 $
                       and line_width gt 15.0 and line_width lt 50.0, $
                     n_east)
                       
        west = where(mask gt 0 and velocity ne 0 and abs(velocity) lt 30 and x gt 0.0  $
                       and intensity[*,*,0] gt 0.2 and intensity[*,*,1] gt 1 and intensity[*,*,2] gt 0.2 $
                       and intensity[*,*,0] lt 60   and intensity[*,*,1] lt 60 and intensity[*,*,2] lt 60 $
                       and line_width gt 15.0 and line_width lt 50.0, $
                     n_west)

        if (method eq 'mean') then begin
          doppler_shift_2  = mean([mean([doppler_shift[east]], /nan), mean([doppler_shift[west]], /nan)], /nan)
          velocity_2       = mean([mean([velocity[east]], /nan), mean([velocity[west]], /nan)], /nan)
          line_width_2     = mean([mean([line_width[east]], /nan), mean([line_width[west]], /nan)], /nan)
          peak_intensity_2 = mean([mean([peak_intensity[east]], /nan),  mean([peak_intensity[west]], /nan)], /nan)
        endif else begin
          doppler_shift_2  = mean([median([doppler_shift[east]]), median([doppler_shift[west]])], /nan)
          velocity_2       = mean([median([velocity[east]]), median([velocity[west]])], /nan)
          line_width_2     = mean([median([line_width[east]]), median([line_width[west]])], /nan)
          peak_intensity_2 = mean([median([peak_intensity[east]]),  median([peak_intensity[west]])], /nan)
        endelse

        ; east/west COMP method 2 - higher intensity thresholds
        east2 = where(mask gt 0 and velocity ne 0 and abs(velocity) lt 30 and x lt 0.0  $
                        and intensity[*,*,0] gt 0.5 and intensity[*,*,1] gt 2 and intensity[*,*,2] gt 0.5 $
                        and intensity[*,*,0] lt 60 and intensity[*,*,1] lt 60 and intensity[*,*,2] lt 60 $
                        and line_width gt 15.0 and line_width lt 50.0, $
                      n_east)

        west2 = where(mask gt 0 and velocity ne 0 and abs(velocity) lt 30 and x gt 0.0  $
                        and intensity[*,*,0] gt 0.5 and intensity[*,*,1] gt 2 and intensity[*,*,2] gt 0.5 $
                        and intensity[*,*,0] lt 60 and intensity[*,*,1] lt 60 and intensity[*,*,2] lt 60 $
                        and line_width gt 15.0 and line_width lt 50.0, $
                      n_west)

        if (method eq 'mean') then begin
          doppler_shift_22  = mean([mean([doppler_shift[east2]], /nan), mean([doppler_shift[west2]], /nan)], /nan)
          velocity_22       = mean([mean([velocity[east2]], /nan), mean([velocity[west2]], /nan)], /nan)
          line_width_22     = mean([mean([line_width[east2]], /nan), mean([line_width[west2]], /nan)], /nan)
          peak_intensity_22 = mean([mean([peak_intensity[east2]], /nan), mean([peak_intensity[west2]], /nan)], /nan)
        endif else begin
          doppler_shift_22  = mean([median([doppler_shift[east2]]), median([doppler_shift[west2]])], /nan)
          velocity_22       = mean([median([velocity[east2]]), median([velocity[west2]])], /nan)
          line_width_22     = mean([median([line_width[east2]]), median([line_width[west2]])], /nan)
          peak_intensity_22 = mean([median([peak_intensity[east2]]), median([peak_intensity[west2]])], /nan)
        endelse

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

        device_east = where(device_mask gt 0 and device_velocity ne 0 and abs(device_velocity) lt 30 and x lt 0.0  $
                              and device_intensity[*,*,0] gt 0.2 and device_intensity[*,*,1] gt 1 and device_intensity[*,*,2] gt 0.2 $
                              and device_intensity[*,*,0] lt 60 and device_intensity[*,*,1] lt 50 and device_intensity[*,*,2] lt 60 $
                              and device_line_width gt 15.0 and device_line_width lt 50.0, $
                            n_east)

        device_west = where(device_mask gt 0 and device_velocity ne 0 and abs(device_velocity) lt 30 and x gt 0.0  $
                              and device_intensity[*,*,0] gt 0.2 and device_intensity[*,*,1] gt 1 and device_intensity[*,*,2] gt 0.2 $
                              and device_intensity[*,*,0] lt 60 and device_intensity[*,*,1] lt 60 and device_intensity[*,*,2] lt 60 $
                              and device_line_width gt 15.0 and device_line_width lt 50.0, $
                            n_west)

        if (method eq 'mean') then begin
          doppler_shift_3  = mean([mean([device_doppler_shift[device_east]], /nan),  mean([device_doppler_shift[device_west]], /nan)], /nan)
          velocity_3       = mean([mean([device_velocity[device_east]], /nan),  mean([device_velocity[device_west]], /nan)], /nan)
          line_width_3     = mean([mean([device_line_width[device_east]], /nan), mean([device_line_width[device_west]], /nan)], /nan)
          peak_intensity_3 = mean([mean([device_peak_intensity[device_east]], /nan), mean([device_peak_intensity[device_west]], /nan)], /nan)
        endif else begin
          doppler_shift_3  = mean([median([device_doppler_shift[device_east]]),  median([device_doppler_shift[device_west]])], /nan)
          velocity_3       = mean([median([device_velocity[device_east]]),  median([device_velocity[device_west]])], /nan)
          line_width_3     = mean([median([device_line_width[device_east]]), median([device_line_width[device_west]])], /nan)
          peak_intensity_3 = mean([median([device_peak_intensity[device_east]]), median([device_peak_intensity[device_west]])], /nan)
        endelse

        ; MUST SAVE ALL QUANTITIES SO WE CAN PLOT THEM A FUNCTION OF TIME
        ; CODE FROM MIKE'S UCOMP ROUTINE     - MIKE:PLEASE CHECK THIS
        printf, lun, date, $
          doppler_shift_0, velocity_0, line_width_0, peak_intensity_0, $
          doppler_shift_1, velocity_1, line_width_1, peak_intensity_1, $
          doppler_shift_2, velocity_2, line_width_2, peak_intensity_2, $
          doppler_shift_22, velocity_22, line_width_22, peak_intensity_22, $
          doppler_shift_3, velocity_3, line_width_3, peak_intensity_3, $
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
