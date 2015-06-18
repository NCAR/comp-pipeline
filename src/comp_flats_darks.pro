; docformat='rst'

;+
; Applies flats and dark frames to an array of CoMP images at various
; wavelengths and polarization states.
;
; :Uses:
;   comp_inventory_header, comp_dark_interp, comp_read_flats, sxpar, sgn,
;   sxaddpar
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
; :Author:
;   Joseph Plowman
;-
pro comp_flats_darks, images, headers, date_dir
  compile_opt strictarr

  ; figure out what's in our image array
  comp_inventory_header, headers, beam, group, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret

  ; convert time format for use by read_flats
  time = comp_extract_time(headers)
  n_ext = n_elements(headers[0, *])
  ntags = n_elements(headers[*, 0])
  if (sxpar(headers[*,0], 'FLATFILE') eq 0) then ntags++
  headersout = strarr(ntags, n_ext)

  ; get the flats and darks
  dark = comp_dark_interp(date_dir, time, expose)
  comp_read_flats, date_dir, wave, beam, time, flat, flat_header, flat_waves, $
                   flat_names, flat_expose
  flat *= expose/ flat_expose   ; modify for exposure times

  for i = 0L, n_ext - 1L do begin   ; loop over the images...
    header = headers[*, i]

    ; select the correct flat for this image
    iflat = where(abs(flat_waves) eq wave[i] and sgn(flat_waves) eq beam[i])

    ; subtract darks, fix sensor quirks, and divide by the flats
    images[*, *, i] = comp_fix_hot(comp_fix_image(comp_fixrock(images[*, *, i] - dark, 0.030)) / flat[*, *, iflat])

    ; update the header with the flat information
    sxaddpar, header,'FLATFILE', flat_names[iflat[0]], $
              ' Name of flat field file'
    headersout[*, i] = header
  endfor

  headers = headersout
end
  