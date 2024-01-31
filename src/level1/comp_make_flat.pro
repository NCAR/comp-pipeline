; docformat = 'rst'

;+
; Procedure to read CoMP uncompressed Level 0 opal data for a day and create a
; file with average flats. This routine reads the `YYYYMMDD.comp.opal.files.txt`
; file that was created by `COMP_FILE_TYPE`.
;
; File `YYYYMMDD.comp.flat.fts` is written to the process directory and a copy
; is written to the Flat directory. Each extension of the flat FITS file
; contains an average flat image for a single wavelength and beam. There are
; also three 1-dimensional extensions written to flat FITS file that contain the
; time the flats were taken, the wavelength of the flats and the exposure times.
; The wavelength contained in the extension is actually the product of the beam
; (+1 or -1) and the wavelength that allows tracking of the beam number.
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
;   error : out, optional, type=long
;     set to a named variable to return the error status of the routine, 0 for
;     success, anything else for failure
;
; :Author:
;   MLSO Software Team
;-
pro comp_make_flat, date_dir, error=error
  compile_opt idl2
  @comp_constants_common
  @comp_config_common
  @comp_fit_common
  @comp_flats_common
  @comp_diagnostics_common
  @comp_check_common

  debug = 0

  mg_log, 'starting', name='comp', /info

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

  ; spectral correction ('yes' or 'no') to normalize by solar spectrum
  spectral_correction = make_flat_spectral_correction

  ; compute current transmission value
  norm = degrade_diffuser ? comp_transmission(date_dir) : 84.0

  ; create arrays
  times = fltarr(5000)
  wavelengths = fltarr(5000)
  exposures = fltarr(5000)

  ; defines hot and adjacent variables
  restore, filename=hot_file

  ; open file for flats
  outfile = string(date_dir, format='(%"%s.comp.flat.fts")')

  fits_open, outfile, fcbout, /write

  ; make a FITS primary header for the flat file
  mkhdr, primary_header, '', /extend

  sxaddpar, primary_header, 'ORIGIN', 'HAO/NCAR'
  sxaddpar, primary_header, 'INSTRUME', 'COMP'
  sxaddpar, primary_header, 'TELESCOP', '20 CM ONE SHOT', $
            ' CORONAL MULTICHANNEL POLARIMETER'
  sxaddpar, primary_header, 'LOCATION', 'Boulder, CO  USA'
  sxaddpar, primary_header, 'DATATYPE', 'FLAT', ' Flat field image'
  sxaddpar, primary_header, 'DETREND', make_flat_detrending ? 'YES' : 'NO', $
            ' Detrend flat'
  sxaddpar, primary_header, 'DESTRAY', make_flat_destraying ? 'YES' : 'NO', $
            ' Remove stray light from flat'
  sxaddpar, primary_header, 'NORMALIZ', norm, $
            ' Transmission of diffuser millionths B/Bsun', $
            format='(F0.3)'
  sxaddpar, primary_header, 'VERSION', code_version, $
            ' Calibration processing software version'
  sxaddpar, primary_header, 'REVISION', code_revision, $
            ' Calibration processing software revision'
  sxaddpar, primary_header, 'BRANCH', code_branch, $
            ' Calibration processing software branch'

  fits_write, fcbout, 0, primary_header

  ;  open list of opal images
  opal_files = string(date_dir, format='(%"%s.comp.opal.files.txt")')
  if (~file_test(opal_files)) then begin
    mg_log, '%s not found', opal_files, name='comp', /critial
  endif
  openr, opal_lun, opal_files, /get_lun

  ; loop through opal files and compute flats
  nflat = 0

  if (cache_flats) then begin
    flat_images_list = list()
    flat_headers_list = list()
  endif

  while (not eof(opal_lun)) do begin
    opalfile = ''
    readf, opal_lun, opalfile, format='(a19)'

    ; record the filename of the current flat for reporting later
    current_flatname = opalfile

    ; open flat file and average images at each wavelength
    fits_open, filepath(opalfile, root=raw_dir), fcbin
    num = fcbin.nextend   ; number of images in file
    fits_read, fcbin, dat, header_opal, /header_only, exten_no=0, /no_abort, message=message
    if (message ne '') then begin
      mg_log, 'error reading %s: %s', opalfile, message, name='comp', /error
      mg_log, 'skipping %s', opalfile, name='comp', /error
      continue
    endif

    time = comp_parse_time(sxpar(header_opal, 'TIME_OBS'), $
                           hours=hours, minutes=minutes, seconds=seconds)

    mg_log, '%02d:%02d:%02d %d images', hours, minutes, seconds, num, $
            name='comp', /info

    ; compute average flat at each wavelength

    ; take inventory of flat file
    comp_inventory, fcbin, beam, wave, pol, type, expose, cover, cal_pol, $
                    error=error
    if (error gt 0L) then begin
      mg_log, 'skipping %s', opalfile, name='comp', /error
      continue
    endif
    
    if (make_flat_beam_multiplies_wave) then begin
      ; multiply wavelength by beam sign to allow to find unique
      ; wavelengths/beams
      wave *= float(beam)
    endif

    ; find unique wavelengths/beams
    uniq_waves = wave[comp_uniq(wave, sort(wave))]
    nwaves = n_elements(uniq_waves)

    wave_type = comp_find_wave_type(wave[0])

    ; perform averaging
    comp_flat_avg, date_dir, time, wave, uniq_waves, exposure, fcbin, flats, nd_filter, $
                   error=error
    if (error gt 0L) then begin
      mg_log, 'error reading averaging %s', opalfile, name='comp', /error
      mg_log, 'skipping %s', opalfile, name='comp', /error
      continue
    endif

    fits_close, fcbin, /no_abort, message=message
    if (message ne '') then begin
      mg_log, 'failed to close %s with: %s', opalfile, message, $
              name='comp', /error
    endif

    ; extract masking information from center wavelength
    ; we have both beam states, i.e. pos and neg wavelength
    ; divide by 4 instead of 2
    image = flats[*, *, nwaves / 4]

    ; fix hot pixels
    image = comp_fix_hot(image, hot=hot, adjacent=adjacent)

    ; make FITS extension header for the images with masking parameters included
    occulter_id = sxpar(header_opal, 'OCC-ID')
    occulter_index = where(occulter_ids eq occulter_id)
    occulter_radius_guess = occulter_radii[occulter_index[0]]

    comp_make_header, image, header, date_dir, occulter_radius_guess, $
                      uncorrected_occulter1, uncorrected_field1, uncorrected_post_angle1, $
                      uncorrected_occulter2, uncorrected_field2, uncorrected_post_angle2, $
                      error=header_error
    if (header_error ne 0L) then begin
      mg_log, 'skipping flat creation for %s', opalfile, name='comp', /warn
      continue
    endif

    sxaddpar, header, 'FILENAME', opalfile, ' Name of raw opal file'
    sxaddpar, header, 'EXPOSURE', exposure, ' Exposure time (millisec)', $
              format='(F0.2)'

    sxaddpar, header, 'WAVELENG', sxpar(header, 'WAVELENG'), $
              ' Wavelength (nm)', format='(F0.3)'
    ext_beam = sxpar(header, 'BEAM')
    sxaddpar, header, 'BEAM', ext_beam, $
              ext_beam gt 0.0 $
                ? ' corona in lower-right, background in upper-left' $
                : ' corona in upper-left, background in lower-right'

    sxaddpar, header, 'NDFILTER', nd_filter, $
              ' ND 1=.1, 2=.3, 3=.5, 4=1, 5=2, 6=3, 7=clr, 8=clr'

    ; should not have the ND filter in while taking a flat; if so, skip
    if (nd_filter ne 8 && wave_type ne '1083') then begin
      mg_log, 'ND %d flat found in %s', nd_filter, opalfile, name='comp', /warn
      continue
    endif

    if (make_flat_spectral_correction eq 0B) then begin
      ; Mask is not wavelength dependent
       mask_full_fill = comp_mask_1024(uncorrected_occulter1, uncorrected_occulter2, $
                                        uncorrected_field1, uncorrected_field2, $
                                        uncorrected_post_angle1, uncorrected_post_angle2, $
                                        o_offset=1.0, f_offset=-3.0 )
    endif

    mg_log, 'waves: %s', strjoin(strtrim(uniq_waves, 2), ', '), $
            name='comp', /debug

    ; Process by wavelength
    for i = 0L, nwaves - 1L do begin
      ; any error will go on to the next image
      catch, error_status
      if (error_status ne 0L) then begin
        mg_log, 'error making flat, skipping this opal image', $
                name='comp', /warn
        mg_log, /last_error, /error, name='comp'
        continue
      endif

      image = flats[*, *, i]

      ; fix hot pixels
      image = comp_fix_hot(image, hot=hot, adjacent=adjacent)

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
        ; use post_angle1 for second post because second is in wrong position
        ; this should be no longer needed 
        comp_fix_trend, image, $
                        uncorrected_occulter1, uncorrected_occulter2, $
                        uncorrected_field1, uncorrected_field2, $
                        uncorrected_post_angle1, uncorrected_post_angle2, fit
        fit_moment = moment(fit)
        sxaddpar, header, 'DETMNFLT', fit_moment[0], ' Detrend Fit Mean for Flat'
        sxaddpar, header, 'DETVRFLT', fit_moment[1], ' Detrend Fit Variance for Flat'
      endif

      ; background correction for the solar spectrum
      if (make_flat_spectral_correction) then begin
        mg_log, 'correcting for the solar spectrum', $
                name='comp', /debug
        comp_flat_norm, abs(uniq_waves[i]), t_on, t_off
        if (uniq_waves[i] lt 0) then begin
          background_correction_1 = t_on
          background_correction_2 = t_off
        endif else begin
          background_correction_1 = t_off
          background_correction_2 = t_on
        endelse

        mask_full_fill = comp_mask_1024(uncorrected_occulter1, uncorrected_occulter2, $
                                        uncorrected_field1, uncorrected_field2, $
                                        uncorrected_post_angle1, uncorrected_post_angle2, $
                                        o_offset=+1.0, f_offset=-3.0, $
                                        bc1=background_correction_1, $
                                        bc2=background_correction_2 )
      endif

      ;  normalize flats so that they are in units of millionths
      image /= norm

      ; Check signal inside the mask which excludes the occulter, field, post, and overlap
      tmp_image = mask_full_fill * image
      medflat = median(tmp_image[where(tmp_image ne 0.)])
      sxaddpar, header, 'MEDIAN', medflat, ' Median value inside annuli', $
                format='(F0.3)'

      ; the flat can be blocked by the dome or the sky conditions could limit
      ; the lights, which lowers the value of the flat
      transmission_correction = comp_correct_nd(nd_filter, 8, uniq_waves[i])
      threshold = (norm - min_flat_median_offset) * expose / 250.0 / transmission_correction

      if (medflat lt threshold) then begin
        mg_log, 'low flat median for %s (%0.2f):', $
                opalfile, uniq_waves[i], name='comp', /warn
        mg_log, '  %0.2f (flat median) < %0.2f (minimum theshold)', $
                medflat, threshold, name='comp', /warn
        n_flats_too_low += 1
        continue
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
      if (cache_flats) then begin
        flat_images_list->add, image
        flat_headers_list->add, header
      endif

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
    goto, done
  endif

  ;  write times, wavelengths and exposure times
  mg_log, 'write times and wavelengths', name='comp', /debug

  times = times[0L:nflat - 1L]
  wavelengths = wavelengths[0L:nflat - 1L]
  exposures = exposures[0L:nflat - 1L]

  if (cache_flats) then begin
    flat_images = flat_images_list->toArray()
    flat_images = transpose(flat_images, [1, 2, 0])
    flat_headers = flat_headers_list->toArray()
    flat_headers = transpose(flat_headers, [1, 0])

    flat_times = times
    flat_wavelengths = wavelengths
    flat_exposures = exposures
    flat_normalize = norm
  endif

  ; remove flat-specific FITS keywords to prepare header for use for an L1 file
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
  sxdelpar, header, 'MEDIAN'

  sxaddpar, header, 'DATATYPE', 'TIMES'
  fits_write, fcbout, times, header, extname='Time'

  sxaddpar, header, 'DATATYPE', 'WAVELENGTHS'
  fits_write, fcbout, wavelengths, header, extname='Wavelength'

  sxaddpar, header, 'DATATYPE', 'EXPOSURES'
  fits_write, fcbout, exposures, header, extname='Exposure'

  done:
  fits_close, fcbout
  mg_log, 'done', name='comp', /info
end
