; docformat = 'rst'

;+
; Continuum correct the flat file for the given date.
;
; :Params:
;   date : in, required, type=string
;     date in the form 'YYYYMMDD'
;-
pro comp_continuum_correction, date
  compile_opt strictarr
  on_error, 2
  @comp_config_common
  @comp_flats_common

  mg_log, 'starting', name='comp', /info

  ; read flat file
  flat_filename = filepath(string(date, format='(%"%s.comp.flat.fts")'), $
                           subdir=[date, 'level1'], $
                           root=process_basedir)

  fits_open, flat_filename, fcb
  n_flats = fcb.nextend - 3
  fits_read, fcb, primary_data, primary_header, exten_no=0

  images = list()
  headers = list()

  for e = 1, n_flats do begin
    fits_read, fcb, data, header, exten_no=e, /pdu, /no_abort, message=msg
    if (msg ne '') then message, msg

    images->add, data
    headers->add, header
  endfor

  fits_read, fcb, flat_time, time_header, exten_no=fcb.nextend - 2
  fits_read, fcb, flat_wavelength, wavelength_header, exten_no=fcb.nextend - 1
  fits_read, fcb, flat_exposure, exposure_header, exten_no=fcb.nextend

  flat_wavelength = abs(flat_wavelength)

  fits_close, fcb

  flat_type = strarr(n_flats)
  for f = 0L, n_flats - 1L do begin
    flat_type[f] = comp_find_wave_type(flat_wavelength[f], /name)
  endfor

  found_correction = bytarr(n_flats)

  chisq_limit = 0.01
  offset_limit = 0.01

  wave_types = ['1074', '1079']
  center_wavelengths = [1074.7, 1079.8]
  for cw = 0L, n_elements(center_wavelengths) - 1L do begin
    mg_log, '%0.1f nm flats', center_wavelengths[cw], $
            name='comp', /info

    compute_continuum_correction = 1B
    if (compute_continuum_correction) then begin
      mg_log, 'computing continuum correction', $
              name='comp', /info
      comp_calibrate_wavelength_2, date, $
                                   wave_types[cw], $
                                   center_wavelengths[cw], $
                                   offset=offset, $
                                   n_flats=n_11pt_flats, $
                                   flat_times=flat_times_11pt, $
                                   wavelengths=wavelengths, $
                                   correction_factors=correction_factors, $
                                   chisq=chisq
    endif else begin
      mg_log, 'looking up continuum correction', $
              name='comp', /info
      comp_lookup_continuum_correction, date, wave_types[cw], $
                                        offset=offset, $
                                        n_flats=n_11pt_flats, $
                                        flat_times=flat_times_11pt, $
                                        wavelengths=wavelengths, $
                                        correction_factors=correction_factors, $
                                        chisq=chisq
    endelse

    if (n_11pt_flats eq 0L) then begin
      mg_log, 'no 11 pt flats fit, skipping', name='comp', /debug
      continue
    endif else begin
      mg_log, '%d sets of 11 pt flats fit', n_11pt_flats, $
              name='comp', /debug
    endelse

    ; use 11 pt flats to correct other flats
    n_wavelengths = 11L

    ; which 11 pt flat indices to use for a given flat
    cor_ind = value_locate(flat_times_11pt, flat_time)

    mg_log, 'apply to %d flats', n_flats, $
            name='comp', /debug

    ; correct matching extensions if chisq/offset are OK
    for f = 0L, n_flats - 1L do begin
      if (wave_types[cw] ne flat_type[f]) then continue

      mg_log, '%0.2f nm, type: %s', $
              flat_wavelength[f], wave_types[cw], $
              name='comp', /debug

      if ((max(chisq[cor_ind[f], *]) lt chisq_limit) $
             && (abs(offset[cor_ind[f], 0] - offset[cor_ind[f], 1]) lt offset_limit)) then begin
        mg_log, '%0.1f nm [flat ext %d/%d]: OK', $
                flat_wavelength[f], f + 1, n_flats, $
                name='comp', /info

        ind = where(abs(wavelengths[cor_ind[f], *] - flat_wavelength[f]) lt 0.001, count)
        if (count eq 0) then begin
          correction = 1.0
          mg_log, 'default correction: %0.4f', correction, name='comp', /debug

          offset1 = 0.0
          offset2 = 0.0
        endif else begin
          correction = correction_factors[cor_ind[f], ind[0]]
          mg_log, 'correction: %0.4f', correction, name='comp', /debug

          offset1 = offset[cor_ind[f], 0]
          offset2 = offset[cor_ind[f], 1]
        endelse
      endif else begin
        ; use default correction and offsets
        correction = 1.0
        offset1    = 0.0
        offset2    = 0.0

        mg_log, '%0.1f nm [flat ext %d/%d]: bad', $
                center_wavelengths[cw], f + 1, n_flats, $
                name='comp', /warn

        if (max(chisq[cor_ind[f], *]) gt chisq_limit) then begin
          mg_log, 'chi-sq %0.3f > %0.3f', $
                  max(chisq[cor_ind[f], *]), chisq_limit, $
                  name='comp', /warn
        endif

        if (abs(offset[cor_ind[f], 0] - offset[cor_ind[f], 1]) gt offset_limit) then begin
          mg_log, 'offset diff %0.3f > %0.3f', $
                  abs(offset[cor_ind[f], 0] - offset[cor_ind[f], 1]), $
                  offset_limit, $
                  name='comp', /warn
        endif
      endelse

      images[f] /= correction
      ; TODO: get final keyword names and comments
      h = headers[f]
      sxaddpar, h, 'CONTCORR', correction, $
                ' continuum emission corr. to flat, eg, H2O vapor', $
                format='(F0.4)'
      sxaddpar, h, 'CONTOFF1', offset1, $
                ' wavelength offset for beam 1', $
                format='(F0.4)'
      sxaddpar, h, 'CONTOFF2', offset2, $
                ' wavelength offset for beam 2', $
                format='(F0.4)'
      headers[f] = h

      found_correction[f] = 1B
    endfor
  endfor

  ; put a default 1.0 in the 1083 flat headers
  not_found_indices = where(found_correction eq 0B, n_not_found)
  for f = 0L, n_not_found - 1L do begin
    h = headers[not_found_indices[f]]
    sxaddpar, h, 'CONTCORR', 1.0, ' continuum emission corr. to flat, eg, H2O vapor', $
              format='(F0.4)'
    sxaddpar, h, 'CONTOFF1', 0.0, $
              ' wavelength offset for beam 1', $
              format='(F0.4)'
    sxaddpar, h, 'CONTOFF2', 0.0, $
              ' wavelength offset for beam 2', $
              format='(F0.4)'
    headers[not_found_indices[f]] = h
  endfor

  ; write flat file

  file_copy, flat_filename, $
             filepath(file_basename(flat_filename, '.fts') + '.original.fts', $
                      root=file_dirname(flat_filename)), $
             /overwrite

  fits_open, flat_filename, fcb, /write
  fits_write, fcb, primary_data, primary_header
  for e = 1, n_flats do begin
    extname = string(flat_wavelength[e - 1], format='(f8.2)')
    fits_write, fcb, images[e - 1], headers[e - 1], extname=extname
  endfor

  fits_write, fcb, flat_time, time_header, extname='Time'
  fits_write, fcb, flat_wavelength, wavelength_header, extname='Wavelength'
  fits_write, fcb, flat_exposure, exposure_header, extname='Exposure'

  fits_close, fcb

  if (cache_flats) then begin
    flat_headers = headers->toArray()
    flat_headers = transpose(flat_headers, [1, 0])
  endif

  done:
  if (obj_valid(headers)) then obj_destroy, headers
  if (obj_valid(images)) then obj_destroy, images

  mg_log, 'done', name='comp', /info
end


; main-level example program

date = '20170105'
comp_initialize, date
config_filename = filepath('comp.mgalloy.compdata.continuum.cfg', $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())
comp_configuration, config_filename=config_filename

comp_continuum_correction, date

;@comp_config_common
;original_flat_filename = filepath(string(date, format='(%"%s.comp.flat.fts")'), $
;                                  subdir=[date, 'level1'], $
;                                  root=process_basedir)
;corrected_flat_filename = filepath(string(date, format='(%"%s.comp.flat.corrected.fts")'), $
;                                  subdir=[date, 'level1'], $
;                                  root=process_basedir)


;diff = mg_fits_diff(original_flat_filename, corrected_flat_filename, $
;                    differences=differences, $
;                    error_msg=msg)
;help, diff, differences
;print, msg

end
