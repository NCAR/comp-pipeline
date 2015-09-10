; docformat = 'rst'

;+
; Procedure to read flat images.
;
; :Uses:
;   comp_uniq, comp_constants_common, comp_paths_common, fits_open, fits_read,
;   fits_close, sxpar, mg_log
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave
;     array of wavelengths of data images
;   beam
;     array of beam values for data images
;   time
;     time of data image
;   flat
;   flat_waves
;   exposure
;
; :Keywords:
;    file
;
; :Author:
;   Tomczyk
;-
pro comp_read_flats, date_dir, wave, beam, time, flat, flat_header, $
                     flat_waves, flat_names, exposure, file=file
  compile_opt idl2
  @comp_constants_common
  @comp_config_common

  process_dir = filepath(date_dir, root=process_basedir)

  ; multiply beam times wave to get unique waves
  beam_multiplies_wave = read_flats_beam_multiplies_wave

  if (beam_multiplies_wave) then begin
    wb = wave * beam
  endif else begin
    wb = wave
  endelse

  flat_waves = wb[comp_uniq(wb, sort(wb))]
  nwave = n_elements(flat_waves)
  mg_log, 'flat_waves: %s', strjoin(strtrim(flat_waves, 2), ', '), $
          name='comp', /debug

  flat = fltarr(1024, 1024, nwave, /nozero)
  flat_names = strarr(nwave)

  ; flat field filename
  if (keyword_set(file)) then begin
    flatfile = filepath(file, root=process_dir)
  endif else begin
    flatfile = filepath('flat.fts', root=process_dir)
  endelse
  fits_open, flatfile, fcb
  num = fcb.nextend

  ; read arrays with times, wavelengths and polarizations
  fits_read, fcb, times, exten_no=num - 2
  fits_read, fcb, wavelengths, exten_no=num - 1
  fits_read, fcb, exposures, exten_no=num

  dt = time - times   ; find time difference from flat times
  bad = where(dt lt 0., count)
  if (count gt 0L) then dt[bad] = 1000.  ; use only flats before time

  ; use closest flat in time with correct wavelength and polarization state
  for iw = 0L, nwave - 1L do begin
    correct_wave = where(flat_waves[iw] eq wavelengths, count)
    if (count eq 0L) then begin
      mg_log, 'no correct_wave for %f in %s', flat_waves[iw], flatfile, $
              name='comp', /warn
      continue
    endif

    mg_log, 'correct_wave: %s', strjoin(strtrim(correct_wave, 2), ', '), $
            name='comp', /debug
    mn = min(dt[correct_wave], good)
    iflat = correct_wave[good] + 1

    fits_read, fcb, dat, flat_header, exten_no=iflat
    ; make sure there aren't any zeros
    bad = where(dat eq 0.0, count)
    if (count gt 0L) then begin
      mg_log, 'zeros in flat at pixels %s', strjoin(strtrim(bad, 2), ', '), $
              name='comp', /warn
    endif

    flat[*, *, iw] = float(dat)
    flat_names[iw] = sxpar(flat_header, 'FILENAME')

    exposure = exposures[iflat - 1L]
    mg_log, 'iflat = %d, flat_waves[iw] = %f, times[iflat - 1] = %f, ' $
              + ' wavelengths[iflat - 1] = %f, exposures[iflat - 1] = %f', $
            iflat, flat_waves[iw], times[iflat - 1], wavelengths[iflat - 1], $
            exposures[iflat - 1], $
            name='comp', /debug
  endfor
  fits_close, fcb
end
