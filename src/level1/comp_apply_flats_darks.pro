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
;
; :Author:
;   Joseph Plowman
;-
pro comp_apply_flats_darks, images, headers, date_dir, flat_header=flat_header
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
  ntags++   ; for the ND-TRANS tag we add below
  headersout = strarr(ntags, n_ext)

  ; get the flats and darks
  dark = comp_dark_interp(date_dir, time, expose)
  comp_read_flats, date_dir, wave, beam, time, flat, flat_header, flat_waves, $
                   flat_names, flat_expose
  flat *= expose / flat_expose   ; modify for exposure times
  flat_nd = sxpar(flat_header, 'NDFILTER', count=flat_nd_present)
  if (~flat_nd_present) then flat_nd = 8

  ; defines hot and adjacent variables
  restore, filename=hot_file

  for i = 0L, n_ext - 1L do begin   ; loop over the images...
    header = headers[*, i]

    ; select the correct flat for this image
    iflat = where(abs(flat_waves) eq wave[i] and sgn(flat_waves) eq beam[i])

    ; subtract darks, fix sensor quirks, and divide by the flats
    images[*, *, i] = comp_fix_hot(comp_fix_image(comp_fixrock(images[*, *, i] - dark, 0.030)) / flat[*, *, iflat], $
                                   hot=hot, adjacent=adjacent)

    nd = sxpar(header, 'NDFILTER', count=nd_present)
    if (~nd_present) then nd = 8
    transmission_correction = comp_correct_nd(nd, flat_nd, wave[i])
    images[*, *, i] *= transmission_correction
    mg_log, 'applying transmission correction of %0.3f on ND %d (flat ND %d)', $
            transmission_correction, nd, nd_flat, name='comp', /debug

    ; update the header with the flat information
    sxaddpar, header, 'ND-TRANS', transmission_correction, $
              ' Mult. factor=transmission of flat ND/img ND', after='NDFILTER'
    sxaddpar, header, 'FLATFILE', flat_names[iflat[0]], $
              ' Name of flat field file'
    headersout[*, i] = header
  endfor

  headers = headersout
end
  
