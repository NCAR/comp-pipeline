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

  ; read flat file
  flat_filename = filepath(string(date, format='(%"%s.comp.flat.fts")'), $
                           subdir=[date, 'level1'], $
                           root=process_basedir)

  fits_open, flat_filename, fcb
  n_flats = fcb.nextend - 3
  wave = fltarr(n_flats)
  beam = lonarr(n_flats)
  fits_read, fcb, primary_data, primary_header, exten_no=0

  images = list()
  headers = list()

  for e = 1, n_flats do begin
    fits_read, fcb, data, header, exten_no=e, /pdu, /no_abort, message=msg
    if (msg ne '') then message, msg

    images->add, data
    headers->add, header
    wave[e - 1] = sxpar(header, 'WAVELENG')
    beam[e - 1] = sxpar(header, 'BEAM')
  endfor

  fits_read, fcb, flat_time, time_header, exten_no=fcb.nextend - 2
  fits_read, fcb, flat_wavelength, wavelength_header, exten_no=fcb.nextend - 1
  fits_read, fcb, flat_exposure, exposure_header, exten_no=fcb.nextend

  fits_close, fcb

  center_wavelengths = [1074.7, 1079.8]
  for w = 0L, n_elements(wavelengths) - 1L do begin
    comp_calibrate_wavelength_2, date, center_wavelengths[w], $
                                 offset=offset, $
                                 n_flats=n_11pt_flats, $
                                 flat_times=flat_times, $
                                 wavelengths=wavelengths, $
                                 correction_factors=correction_factors, $
                                 chisq=chisq
    ; TODO: correct matching extensions if chisq/offset are OK
  endfor

  ; write flat file

  ; TODO: for now, write a new file instead of overwriting the existing file
  flat_filename = filepath(file_basename(flat_filename, '.fts') + '.corrected.fts', $
                           root='.')

  fits_open, flat_filename, fcb, /write
  fits_write, fcb, primary_data, primary_header
  for e = 1, n_flats do begin
    extname = string(beam[e - 1] * wave[e - 1], format='(f8.2)')
    fits_write, fcb, images[e - 1], headers[e - 1], extname=extname
  endfor

  fits_write, fcb, flat_time, time_header, extname='Time'
  fits_write, fcb, flat_wavelength, wavelength_header, extname='Wavelength'
  fits_write, fcb, flat_exposure, exposure_header, extname='Exposure'

  fits_close, fcb

  done:
  if (obj_valid(headers)) then obj_destroy, headers
  if (obj_valid(images)) then obj_destroy, images

  mg_log, 'done', name='comp', /info
end


; main-level example program

date = '20130115'
comp_initialize, date
config_filename = filepath('comp.mgalloy.mahi.latest.cfg', $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())
comp_configuration, config_filename=config_filename

comp_continuum_correction, date

@comp_config_common
original_flat_filename = filepath(string(date, format='(%"%s.comp.flat.fts")'), $
                                  subdir=[date, 'level1'], $
                                  root=process_basedir)
corrected_flat_filename = filepath(string(date, format='(%"%s.comp.flat.corrected.fts")'), $
                                   root='.')

diff = mg_fits_diff(original_flat_filename, corrected_flat_filename, $
                    differences=differences, $
                    error_msg=msg)
help, diff, msg, differences

end
