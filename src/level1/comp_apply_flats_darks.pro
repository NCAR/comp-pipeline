; docformat='rst'

;+
; Applies flats and dark frames to an array of CoMP images at various
; wavelengths and polarization states.
;
; :Uses:
;   comp_config_common, comp_inventory_header, comp_extract_time,
;   comp_dark_interp, comp_read_flats, comp_fix_hot, sxpar, sgn, sxaddpar
;
; :Params:
;   images : in, required, type="fltarr(nx, ny, n_images)"
;     the array of CoMP images. Will be dark and flat corrected on output
;   headers : in, out, required, type="strarr(ntags, n_images)"
;     FITS headers for each of the images; the name of the flat file will be
;     added on output
;   date_dir : in, required, type=string
;     name of directory containing the files for the date of the input images,
;     used to find the appropriate dark and flat files
;
; :Keywords:
;   flat_header : out, optional, type=strarr
;     flat header
;   uncorrected_images : out, optional, type="fltarr(nx, ny, n_images)"
;     the array of CoMP images, not flat corrected, but corrected in other ways
;   error : out, optional, type=long
;     set to a named variable to retrieve whether there was an error in applying
;     flats and darks; 0 indicates no error
;   filename : in, required, type=string
;     filename of raw file being processed
;
; :Author:
;   MLSO Software Team
;-
pro comp_apply_flats_darks, wave_type, images, headers, primary_header, date_dir, $
                            flat_header=flat_header, $
                            uncorrected_images=uncorrected_images, $
                            filename=filename, $
                            error=error
  compile_opt strictarr
  @comp_config_common
  @comp_constants_common

  error = 0L

  uncorrected_images = images

  ; figure out what's in our image array
  comp_inventory_header, headers, beam, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret

  ; convert time format for use by read_flats
  time = comp_extract_time(headers)
  n_ext = n_elements(headers[0, *])
  ntags = n_elements(headers[*, 0])
  original_ntags = n_tags

  optional_tags = ['OBS_ID', 'OBS_PLAN', 'O1FOCUS', 'ND-FILTER']
  hastags = mg_fits_hastag(headers[*, 0], optional_tags, count=n_hastags)

  ntags += n_elements(optional_tags) - n_hastags
  ntags++   ; for RAWEXT tag we add below
  ntags++   ; for the ND-TRANS tag we add below
  ntags++   ; for the FLATFILE tag we add below
  ntags++   ; for the FLATEXT tag we add below
  ntags++   ; for the FLATMED tag we add below
  if (remove_stray_light && wave_type ne '1083') then ntags += 2   ; for FITMNLIN/FITVRLIN

  headersout = strarr(ntags, n_ext)

  ; get the flats and darks
  dark = comp_dark_interp(date_dir, time, expose)
  comp_read_flats, date_dir, wave, beam, time, flat, flat_header, flat_waves, $
                   flat_names, flat_expose, flat_extensions=flat_extensions, $
                   flat_found=flat_found, normalize=normalize
  if (total(flat_found, /integer) eq 0L) then begin
    mg_log, 'no valid flats found', name='comp', /error
    error = 1L
    goto, done
  endif

  sxaddpar, primary_header, 'NORMALIZ', normalize, $
            ' Transmission of diffuser millionths B/Bsun', $
            format='(F0.3)'

;  flat_mask = comp_annulus_1024(flat_header, o_offset=0.0, f_offset=0.0, /uncorrected)
  
  for f = 0L, n_elements(flat_expose) - 1L do begin
    if (flat_found[f]) then begin
      flat[*, *, f] *= expose / flat_expose[f]   ; modify for exposure times
    endif else begin
      mg_log, 'no flat for ext %d', f + 1L, name='comp', /warn
    endelse
  endfor

  wave_type = comp_find_wavelength(wave[0], /name)
  flat_nd = comp_get_nd_filter(date_dir, wave_type, reform(flat_header[*, 0]))

  ; defines hot and adjacent variables
  restore, filename=hot_file

  blank_line = string(bytarr(80) + (byte(' '))[0])

  ; dark and other image corrections (not flat correction)
  for i = 0L, n_ext - 1L do begin   ; loop over the images...
    ; subtract darks, fix sensor quirks, and divide by the flats
    tmp_image  = images[*, *, i]
    tmp_image -= dark
    tmp_image  = comp_fixrock(temporary(tmp_image), 0.030)
    tmp_image  = comp_fix_image(temporary(tmp_image))

    ; check for 3,000 pixels above 10,000
    bad_pixels = where(tmp_image gt quality_threshold, n_bad_pixels)
    if (n_bad_pixels gt quality_count) then begin
      mg_log, 'rejecting ext %d for %d pixels > %0.1f', $
              i + 1, n_bad_pixels, quality_threshold, $
              name='comp', /warn
      ; add filename to bad quality log
      mg_log, file_basename(filename), $
              name=strjoin(['comp', 'bad.quality', wave_type], '/'), $
              /info
      error = 1
      goto, done
    endif

    images[*, *, i] = temporary(tmp_image)
  endfor

  ; add a spot for RAWEXT
  _headers = strarr(orignal_ntags, n_exts)
  _headers[0, 0] = headers
  headers = _headers

  for i = 0L, n_ext - 1L do begin
    ; add RAWEXT keyword that is trivial to begin with
    tmp_header = headers[*, i]
    sxaddpar, tmp_header, 'RAWEXT', strtrim(i + 1, 2), ' exts from raw file used'
    headers[*, i] = tmp_header
  endfor

  ; combine like extensions (with same pol, beam, and wavelength)
  comp_combine_extensions, images, headers, pol, beam, wave, error=combine_error
  if (combine_error ne 0L) then begin
    mg_log, 'problem combining like extensions', name='comp', /error
    error = 2
    goto, done
  endif

  for i = 0L, n_images - 1L do begin
    header = headers[*, i]
    tmp_image = images[*, *, i]

    ; select the correct flat for this image
    iflat = where(abs(flat_waves) eq wave[i] and sgn(flat_waves) eq beam[i])
    iflat = iflat[0]

    if (remove_stray_light && wave_type ne '1083') then begin
      ; using the flat header is probably OK here since we over-occult by so
      ; much in COMP_FIX_STRAY_LIGHT
      comp_fix_stray_light, tmp_image, flat_header[*, iflat], fit, $
                            coefficients=stray_coefficients

      ; write stray light coefficients
      n_coeffs = n_elements(stray_coefficients)
      mg_log, '%s, %d, %0.2f, %s, ' + strjoin(strarr(n_coeffs) + '%0.4g', ', '), $
              file_basename(filename), i + 1, wave[i], pol[i], stray_coefficients, $
              name='stray-light', /info
      mg_log, 'stray light coefficients: %s', $
              strjoin(strjoin(strtrim(stray_coefficients, 2), ', '), ' / '), $
              name='comp', /debug

      ; characterize the fit and save in the header
      fit_moment = moment(fit)
      sxaddpar, header, 'FITMNLIN', fit_moment[0], $
                ' Stray Light Fit Mean for Line'
      sxaddpar, header, 'FITVRLIN', fit_moment[1], $
                ' Stray Light Fit Variance for Line'
    endif

    ; don't flat correct the uncorrected images
    uncorrected_tmp_image = tmp_image
    if (flat_found[iflat]) then begin
      tmp_image /= flat[*, *, iflat]
    endif else begin
      mg_log, 'unable to flat correct extension %d', i + 1L, $
              name='comp', /warn
      error = 1L
      goto, done
    endelse

    tmp_image = comp_fix_hot(temporary(tmp_image), hot=hot, adjacent=adjacent)
    uncorrected_tmp_image = comp_fix_hot(temporary(uncorrected_tmp_image), $
                                         hot=hot, adjacent=adjacent)

    ; store images
    images[*, *, i] = temporary(tmp_image)
    uncorrected_images[*, *, i] = temporary(uncorrected_tmp_image)

    nd = comp_get_nd_filter(date_dir, wave_type, header)
    if (wave_type eq '1074' || wave_type eq '1079') then begin
      transmission_correction = 1.0
      if (nd ne 8) then begin
        mg_log, 'non-ND=8 (%d) found for %s nm image', nd, wave_type, $
                name='comp', /warn
      endif
    endif else begin
      transmission_correction = comp_correct_nd(nd, flat_nd, wave[i])
      images[*, *, i] *= transmission_correction
    endelse

    if (flat_found[iflat]) then begin
      medflat = sxpar(reform(flat_header[*, iflat]), 'MEDIAN')
    endif else medflat = !values.f_nan

    ; update the header with the flat information
    sxaddpar, header, 'ND-TRANS', transmission_correction, $
              ' Mult. factor=transmission of flat ND/img ND', after='NDFILTER'
    sxaddpar, header, 'FLATFILE', flat_names[iflat], $
              ' Name of flat field file'
    sxaddpar, header, 'FLATEXT', flat_extensions[iflat], $
              string(date_dir, $
                     format='(%" Extension in %s.comp.flat.fts used")'), $
              after='FLATFILE'
    sxaddpar, header, 'FLATMED', medflat, $
              ' median of dark and exposure corrected flat', $
              format='(F0.2)', after='FLATEXT'

    nonblank_ind = where(header ne blank_line, n_nonblank)
    header = header[nonblank_ind]
    if (ntags ne n_nonblank) then header = [header, strarr(ntags - n_nonblank) + blank_line]

    headersout[0, i] = reform(header, n_elements(header), 1)
 endfor

  headers = headersout

  done:
end
  
