; docformat = 'rst'

;+
; Procedure to read CoMP uncompressed Level 0 opal data for a day and create a
; file with average flats. This routine reads the `opal_files.txt` file that
; was created by `COMP_FILE_TYPE`.
;
; File `flat.fts` is written to the process directory and a copy is written to
; the Flat directory. Each extension of the `flat.fts` file contains an
; average flat image for a single wavelength and beam. There are also three 1d
; extensions written to `flat.fts` that contains the time the flats were taken,
; the wavelength of the flats and the exposure times. The wavelength contained 
; in the extension is actually the product of the beam (+1 or -1) and the
; wavelength that allows tracking of the beam number.
;
; :Examples:
;   For example, call like::
;
;     comp_make_flat, '20130915'
;
; :Uses:
;   comp_constants_common, comp_config_common, comp_fit_common, comp_initialize,
;   comp_configuration, comp_inventory, comp_mask_1024, comp_fix_stray_light,
;   comp_flat_norm, comp_fix_hot, comp_make_header, comp_annulus_1024,
;   comp_fix_trend, fits_open, fits_close, fits_read, fits_write, sxaddpar,
;   mkhdr, sxdelpar
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;
; :Keywords:
;   replace_flat : in, optional, type=boolean
;     option to replace flat with older one from flat directory
;   error : out, optional, type=long
;     set to a named variable to return the error status of the routine, 0 for
;     success, anything else for failure
;
; :Author: 
;   Tomczyk, modified by Sitongia
;-
pro comp_make_flat, date_dir, replace_flat=replace_flat, error=error
  compile_opt idl2
  @comp_constants_common
  @comp_config_common
  @comp_fit_common

  ; configure
  comp_initialize, date_dir
  comp_configuration

  
  debug = 0

  mg_log, 'starting', name='comp', /info

  ; Establish error handler. When errors occur, the index of the
  ; error is returned in the variable Error_status:
  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    mg_log, /last_error, name='comp'
    return
  endif

  raw_dir = filepath(date_dir, root=raw_basedir)
  process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)
  cd, process_dir

  ans = ' '

  ; set options for processing

  ; beam multiplies wavelength ('yes' or 'no')
  ; 'yes' will multiply beam times wavelength to get unique flats for
  ;   wavelength and beam
  ; 'no' will average flats for the two different beams
  beam_multiplies_wave = make_flat_beam_multiplies_wave

  ; detrending ('yes' or 'no') to remove spatial trends from flat images
  detrending = make_flat_detrending

  ; destraying ('yes' or 'no') to subtract stray light
  destraying = make_flat_destraying   ; remove stray light

  ; fill ('yes' or 'no') to fill region outside flat with fit values?
  fill = make_flat_fill

  ; spectral correction ('yes' or 'no') to normalize by solar spectrum
  spectral_correction = make_flat_spectral_correction

  norm = 84.     ; opal calibration of 84 millionths @ 1074 nm

  ; create arrays
  times = fltarr(5000)
  wavelengths = fltarr(5000)
  exposures = fltarr(5000)

  ; defines hot and adjacent variables
  restore, filename=hot_file

  ; open file for flats
  ; optionally get older flat from flat directory
  if (keyword_set(replace_flat)) then begin
    mg_log, 'replacing flat file with older one'
    file_copy, filepath('flat.fts', root=flat_dir), process_dir, /overwrite
  endif else begin
    outfile = 'flat.fts'

    fits_open, outfile, fcbout, /write

    ; make a FITS primary header for the flat file
    mkhdr, primary_header, '', /extend

    sxaddpar, primary_header, 'ORIGIN', 'HAO/NCAR'
    sxaddpar, primary_header, 'INSTRUME', 'COMP'
    sxaddpar, primary_header, 'TELESCOP', '20 CM ONE SHOT', $
              ' CORONAL MULTICHANNEL POLARIMETER'
    sxaddpar, primary_header, 'LOCATION', 'Boulder, CO  USA'
    sxaddpar, primary_header, 'DATATYPE', 'FLAT', ' Flat field image'
    sxaddpar, primary_header, 'DETREND', make_flat_detrending ? 'YES' : 'NO'
    sxaddpar, primary_header, 'DESTRAY', make_flat_destraying ? 'YES' : 'NO'
    sxaddpar, primary_header, 'NORMALIZ', norm
    sxaddpar, primary_header, 'VERSION', code_version, $
              ' Calibration processing software version'
    sxaddpar, primary_header, 'REVISION', code_revision, $
              ' Calibration processing software revision'

    fits_write, fcbout, 0, primary_header

    ;  open list of opal images
    opal_files = 'opal_files.txt'
    if (~file_test(opal_files)) then begin
      mg_log, '%s not found', opal_files, name='comp', /critial
    endif
    openr, opal_lun, opal_files, /get_lun

    ;  loop through opal files and compute flats
    nflat = 0
    while (not eof(opal_lun)) do begin
      opalfile = ''
      readf, opal_lun, opalfile, format='(a19)'
      mg_log, 'opal %s', opalfile, name='comp', /info

      ; open flat file and average images at each wavelength
      fits_open, filepath(opalfile, root=raw_dir), fcbin
      num = fcbin.nextend   ; number of images in file
      fits_read, fcbin, dat, header, /header_only, exten_no=0

      ; sxdelpar,header,'COMMENT'
      ; sxdelpar,header,'POLSTATE'
      ; sxdelpar,header,'SEQUENCE'

      time = comp_parse_time(sxpar(header, 'TIME_OBS'))

      mg_log, '%d images in file', num, name='comp', /debug

      ; compute average flat at each wavelength

      ; take inventory of flat file
      comp_inventory, fcbin, beam, group, wave, pol, type, expose, cover, cal_pol

      ; test for bad opal against thresholds
      if (wave[0] lt 1075.0) then begin
        threshold = 18.0
      endif else if (wave[0] gt 1080.0) then begin
        threshold = 0.1
      endif else begin
        threshold = 18.0
      endelse

      if (make_flat_beam_multiplies_wave) then begin
        ; multiply wavelength by beam sign to allow to find unique
        ; wavelengths/beams
        wave *= float(beam)
      endif

      ; find unique wavelengths/beams
      uniq_waves = wave[comp_uniq(wave, sort(wave))]
      nwaves = n_elements(uniq_waves)

      ; perform averaging
      comp_flat_avg, date_dir, time, wave, uniq_waves, exposure, fcbin, flats

      ; extract masking information from second flat image (don't use first)
      image = flats[*, *, 1]

      ; fix hot pixels
      image = comp_fix_hot(image, hot=hot, adjacent=adjacent)

      ; make FITS extension header for the images with masking parameters included
      comp_make_header, image, header, $
                        occulter1, field1, post_angle1, $
                        occulter2, field2, post_angle2

      sxaddpar, header, 'FILENAME', opalfile, ' Name of raw opal file'
      sxaddpar, header, 'EXPOSURE', exposure

      if (make_flat_spectral_correction eq 0B) then begin
        ; Mask is not wavelength dependent
        mask_full_fill = comp_annulus_1024(header, o_offset=0.0, f_offset=0.0)
      endif

      ; Process by wavelength
      for i = 0L, nwaves - 1L do begin

        ; any error will go on to the next image
        catch, error_status
        if (error_status ne 0L) then begin
          ;catch, /cancel
          mg_log, 'error making flat, skipping this opal image', $
                  name='comp', /warn
          mg_log, /last_error, name='comp'
          continue
        endif

        image = flats[*, *, i]

        ; fix hot pixels
        image = comp_fix_hot(image, hot=hot, adjacent=adjacent)

        mg_log, 'waves: %s', strjoin(strtrim(uniq_waves, 2), ', '), $
                name='comp', /debug
        if (debug eq 1) then begin
          tvwin, image
          profiles, image
          wait, 0.5
        endif

        sxaddpar, header, 'WAVELENG', abs(uniq_waves[i])
        sxaddpar, header, 'BEAM', fix(uniq_waves[i] / abs(uniq_waves[i]))

        ; corrections for stray light and trending

        ; remove stray light
        if (make_flat_destraying) then begin
          ; doesn't have post and overlap in
          comp_fix_stray_light, image, header, fit
          ; characterize the fit and save
          fit_moment = moment(fit)
          sxaddpar, header, 'FITMNFLT', fit_moment[0], ' Stray Light Fit Mean for Flat'
          sxaddpar, header, 'FITVRFLT', fit_moment[1], ' Stray Light Fit Variance for Flat'
        endif

        ; detrend across large image
        if (make_flat_detrending) then begin
          ; TODO use post_angle1 for second post because second is in wrong position
          comp_fix_trend, image, occulter1, occulter2, field1, field2, post_angle1, post_angle1, fit
          fit_moment = moment(fit)
          sxaddpar, header, 'DETMNFLT', fit_moment[0], ' Detrend Fit Mean for Flat'
          sxaddpar, header, 'DETVRFLT', fit_moment[1], ' Detrend Fit Variance for Flat'
        endif

        ; background correction for the solar spectrum
        if (make_flat_spectral_correction) then begin
          mg_log, 'background correction for the solar spectrum', $
                  name='comp', /info
          comp_flat_norm, abs(uniq_waves[i]), t_on, t_off
          if (uniq_waves[i] lt 0) then begin
            background_correction_1 = t_on
            background_correction_2 = t_off
          endif else begin
            background_correction_1 = t_off
            background_correction_2 = t_on
          endelse

          mask_full_fill = comp_mask_1024(occulter1, occulter2, $
                                          field1, field2, $
                                          post_angle1, post_angle2, $
                                          o_offset=0.0, f_offset=0.0, $
                                          bc1=background_correction_1, $
                                          bc2=background_correction_2, $
                                          /nopost, /nooverlap, /nullcolumns)
        endif

        ;  normalize flats so that they normalize intensity into units of millionths
        image /= norm

        ; Check signal
        ; A mask with only occulter and field, but right at edges
        tmp_image = mask_full_fill * image
        medflat = median(tmp_image[where(tmp_image ne 0.)])

        ; Test medflat for scenario where opal didn't go in after a power failure
        if (medflat lt threshold) then begin
          mg_log, 'flat median too low', name='comp', /warn
          mg_log, 'is the opal failing to go back in?', name='comp', /warn
          break
        endif

        ; make sure there aren't any zeros
        bad = where(image eq 0.0, count)
        if (count gt 0L) then begin
          mg_log, 'zeros in flat %s at pixels %s', opalfile, $
                  strjoin(strtrim(bad, 2), ', '), $
                  name='comp', /warn
          image[bad] = medflat
        endif

        ename = string(format='(f8.2)', uniq_waves[i])

        fits_write, fcbout, image, header, extname=ename

        times[nflat] = time
        wavelengths[nflat] = uniq_waves[i]
        exposures[nflat] = exposure
        ++nflat
      endfor
    endwhile

    free_lun, opal_lun

    if (nflat eq 0L) then begin
      mg_log, 'no flats for this day', name='comp', /critical
      error = 1L
      return
    endif

    ;  write times, wavelengths and exposure times
    mg_log, 'write times and wavelengths', name='comp', /debug

    times = times[0L:nflat - 1L]
    wavelengths = wavelengths[0L:nflat - 1L]
    exposures = exposures[0L:nflat - 1L]

    sxdelpar, header, 'BEAM'
    sxdelpar, header, 'WAVELENG'
    sxdelpar, header, 'OXCNTER1'
    sxdelpar, header, 'OYCNTER1'
    sxdelpar, header, 'ORADIUS1'
    sxdelpar, header, 'OXCNTER2'
    sxdelpar, header, 'OYCNTER2'
    sxdelpar, header, 'ORADIUS2'
    sxdelpar, header, 'FXCNTER1'
    sxdelpar, header, 'FYCNTER1'
    sxdelpar, header, 'FRADIUS1'
    sxdelpar, header, 'FXCNTER2'
    sxdelpar, header, 'FYCNTER2'
    sxdelpar, header, 'FRADIUS2'

    sxaddpar, header, 'DATATYPE', 'TIMES'
    fits_write, fcbout, times, header, extname='Time'

    sxaddpar, header, 'DATATYPE', 'WAVELENGTHS'
    fits_write, fcbout, wavelengths, header, extname='Wavelength'

    sxaddpar, header, 'DATATYPE', 'EXPOSURES'
    fits_write, fcbout, exposures, header, extname='Exposure'

    fits_close, fcbout
  endelse

  mg_log, 'done', name='comp', /info
end
