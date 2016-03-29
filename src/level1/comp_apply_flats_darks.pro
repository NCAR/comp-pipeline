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
;   images : in, out, required, type="fltarr(nx, ny, nimg)"
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
;   flat_images : out, optional, type=fltarr
;     set to a named variable to retrieve the flat images to be used
;   dark_image : out, optional, type=fltarr
;     set to a named variable to retrieve the dark image to be used
;   no_apply : in, optional, type=boolean
;     set to not apply the flats and darks to the image
;
; :Author:
;   Joseph Plowman
;-
pro comp_apply_flats_darks, images, headers, date_dir, $
                            flat_header=flat_header, $
                            flat_images=flat_images, $
                            dark_image=dark_image, $
                            no_apply=no_apply
  compile_opt strictarr
  @comp_config_common

  ; figure out what's in our image array
  comp_inventory_header, headers, beam, group, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret

  ; convert time format for use by read_flats
  time = comp_extract_time(headers)
  n_ext = n_elements(headers[0, *])
  ntags = n_elements(headers[*, 0])
  if (sxpar(headers[*, 0], 'FLATFILE') eq 0) then ntags++
  headersout = strarr(ntags, n_ext)

  ; get the flats and darks
  dark_image = comp_dark_interp(date_dir, time, expose)
  comp_read_flats, date_dir, wave, beam, pol, time, $
                   flat, flat_header, flat_waves, flat_pols, n_uniq_pols, $
                   flat_names, flat_expose
  flat *= expose / flat_expose   ; modify for exposure times

  ; defines hot and adjacent variables
  restore, filename=hot_file

  if (arg_present(flat_images)) then flat_images = fltarr(1024, 1024, n_ext)

  for i = 0L, n_ext - 1L do begin   ; loop over the images...
    header = headers[*, i]

    ; select the correct flat for this image
    ; NOTE: comparing both wavelengths and sign of beam is equivalent
    ; to old BEAM_MULTIPLIES_WAVE option
    w = where(beam[i] * wave[i] eq flat_waves)
    p = where(pol[i] eq flat_pols)
    iflat = w[0] * n_uniq_pols + p[0]

    mg_log, 'Using flat %d in %s', iflat, flat_names[iflat], name='comp', /debug

    ; subtract darks, fix sensor quirks, and divide by the flats
    if (arg_present(flat_images)) then flat_images[*, *, i] = flat[*, *, iflat]

    if (~keyword_set(no_apply)) then begin
      im = images[*, *, i] - dark_image
      im = comp_fixrock(temporary(im), 0.030)
      im = comp_fix_image(temporary(im))
      im = comp_fix_hot(temporary(im) / flat[*, *, iflat], $
                      hot=hot, adjacent=adjacent)
      images[*, *, i] = temporary(im)
    endif

    ; update the header with the flat information
    sxaddpar, header,'FLATFILE', flat_names[iflat], $
              ' Name of flat field file'
    headersout[*, i] = header
  endfor

  headers = headersout
end
  
