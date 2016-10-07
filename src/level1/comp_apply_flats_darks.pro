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
;   images : in, required, type="fltarr(nx, ny, nimg)"
;     the array of CoMP images. Will be dark and flat corrected on output
;   headers : in, out, required, type="strarr(ntags, nimg)"
;     FITS headers for each of the images; the name of the flat file will be
;     added on output
;   date_dir : in, required, type=string
;     name of directory containing the files for the date of the input images,
;     used to find the appropriate dark and flat files
;
; :Keywords:
;   flat_header : out, optional, type=strarr
;     flat header
;   error : out, optional, type=long
;     set to a named variable to retrieve whether there was an error in applying
;     flats and darks; 0 indicates no error
;
; :Author:
;   Joseph Plowman
;-
pro comp_apply_flats_darks, images, headers, date_dir, flat_header=flat_header, error=error
  compile_opt strictarr
  @comp_config_common

  error = 0L

  ; figure out what's in our image array
  comp_inventory_header, headers, beam, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret

  ; convert time format for use by read_flats
  time = comp_extract_time(headers)
  n_ext = n_elements(headers[0, *])
  ntags = n_elements(headers[*, 0])
  optional_tags = ['OBS_ID', 'OBS_PLAN', 'O1FOCUS', 'ND-FILTER', 'FLATFILE']
  hastags = mg_fits_hastag(headers[*, 0], optional_tags, count=n_hastags)
  ntags += n_elements(optional_tags) - n_hastags
  ntags++   ; for the ND-TRANS tag we add below
  ntags++   ; for the FLATEXT tag we add below
  ntags++   ; for the FLATMED tag we add below
  if (remove_stray_light) then ntags += 2   ; for FITMNLIN/FITVRLIN
  headersout = strarr(ntags, n_ext)

  ; get the flats and darks
  dark = comp_dark_interp(date_dir, time, expose)
  comp_read_flats, date_dir, wave, beam, time, flat, flat_header, flat_waves, $
                   flat_names, flat_expose, flat_extensions=flat_extensions, $
                   flat_found=flat_found
  if (total(flat_found, /integer) eq 0L) then begin
    mg_log, 'no valid flats found', name='comp', /error
    error = 1L
    return
  endif

  flat_mask = comp_annulus_1024(flat_header, o_offset=0.0, f_offset=0.0)

  for f = 0L, n_elements(flat_expose) - 1L do begin
    if (flat_found[f]) then begin
      flat[*, *, f] *= expose / flat_expose[f]   ; modify for exposure times
    endif
  endfor

  wave_type = comp_find_wavelength(wave[0], /name)
  flat_nd = comp_get_nd_filter(date_dir, wave_type, flat_header)

  ; defines hot and adjacent variables
  restore, filename=hot_file

  for i = 0L, n_ext - 1L do begin   ; loop over the images...
    header = headers[*, i]

    ; select the correct flat for this image
    iflat = where(abs(flat_waves) eq wave[i] and sgn(flat_waves) eq beam[i])

    ; subtract darks, fix sensor quirks, and divide by the flats
    tmp_image  = images[*, *, i]
    tmp_image -= dark
    tmp_image  = comp_fixrock(temporary(tmp_image), 0.030)
    tmp_image  = comp_fix_image(temporary(tmp_image))

    if (remove_stray_light) then begin
      comp_fix_stray_light, tmp_image, header, fit

      ; characterize the fit and save in the header
      fit_moment = moment(fit)
      sxaddpar, header, 'FITMNLIN', fit_moment[0], $
                ' Stray Light Fit Mean for Line'
      sxaddpar, header, 'FITVRLIN', fit_moment[1], $
                ' Stray Light Fit Variance for Line'
    endif

    if (flat_found[iflat]) then begin
      tmp_image /= flat[*, *, iflat]
    endif
    tmp_image = comp_fix_hot(temporary(tmp_image), hot=hot, adjacent=adjacent)
    images[*, *, i] = temporary(tmp_image)

    nd = comp_get_nd_filter(date_dir, wave_type, header)
    transmission_correction = comp_correct_nd(nd, flat_nd, wave[i])
    images[*, *, i] *= transmission_correction

    if (flat_found[iflat]) then begin
      flat_image = flat[*, *, iflat] * flat_mask
      medflat = median(flat_image[where(flat_image ne 0.0)])
    endif else medflat = !values.f_nan

    ; update the header with the flat information
    sxaddpar, header, 'ND-TRANS', transmission_correction, $
              ' Mult. factor=transmission of flat ND/img ND', after='NDFILTER'
    sxaddpar, header, 'FLATFILE', flat_names[iflat[0]], $
              ' Name of flat field file'
    sxaddpar, header, 'FLATEXT', flat_extensions[iflat[0]], $
              ' Extension in flat.fts (not FLATFILE) used', after='FLATFILE'
    sxaddpar, header, 'FLATMED', medflat, $
              ' median of dark and exposure corrected flat', after='FLATEXT'

    headersout[0, i] = reform(header, n_elements(header), 1)
  endfor

  headers = headersout
end
  
