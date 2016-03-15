; docformat = 'rst'

;+
; Procedure to read flat images.
;
; :Uses:
;   comp_uniq, comp_constants_common, comp_config_common, fits_open, fits_read,
;   fits_close, sxpar, mg_log
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave : in, required, type=fltarr(n_extensions)
;     array of wavelengths of data images
;   beam : in, required, type=fltarr(n_extensions)
;     array of beam values for data images
;   time : in, required, type=fltarr(n_extensions)
;     time of data image
;   flat : out, required, type="fltarr(1024, 1024, nflat)"
;     set to a named variable to retrieve the flat images
;   flat_header : out, optional, type=strarr
;   flat_waves : out, optional, type=fltarr(nflat)
;   flat_pols : out, optional, type=fltarr(nflat)
;   n_uniq_pols : out, optional, type=integer
;   flat_names : out, optional, type=strarr(nflat)
;     filename of used flat
;   exposure : out, optional, type=float
;     exposure time
;
; :Keywords:
;   file : in, optional, type=string, default=flat.fts
;     filename of FITS file containing flats
;
; :Author:
;   Tomczyk
;-
pro comp_read_flats, date_dir, wave, beam, pol, time, flat, flat_header, $
                     flat_waves, flat_pols, n_uniq_pols, flat_names, exposure, $
                     file=file
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
  n_uniq_waves = n_elements(flat_waves)

  flat_pols = pol[comp_uniq(pol, sort(pol))]
  n_uniq_pols = n_elements(flat_pols)

  mg_log, 'flat_waves: %s', strjoin(strtrim(flat_waves, 2), ', '), $
          name='comp', /debug

  flat = fltarr(1024, 1024, n_uniq_waves * n_uniq_pols, /nozero)
  flat_names = strarr(n_uniq_waves * n_uniq_pols)

  ; flat field filename
  if (n_elements(file) gt 0L) then begin
    flatfile = filepath(file, root=process_dir)
  endif else begin
    flatfile = filepath('flat.fts', root=process_dir)
  endelse

  fits_open, flatfile, fcb
  num = fcb.nextend

  ; read arrays with times, wavelengths, exposures, and polarizations
  fits_read, fcb, times, exten_no=num - 3
  fits_read, fcb, wavelengths, exten_no=num - 2
  fits_read, fcb, exposures, exten_no=num - 1
  fits_read, fcb, pols_indices, exten_no=num

  pol_names = 'I' + ['+Q', '-Q', '+U', '-U', '+V', '-V']
  pols = pol_names[pols_indices]

  dt = time - times   ; find time difference from flat times
  bad = where(dt lt 0., count)
  if (count gt 0L) then dt[bad] = 1000.  ; penalize flats before time

  ; use closest flat in time with correct wavelength and polarization state
  for w = 0L, n_uniq_waves - 1L do begin
    for p = 0L, n_uniq_pols - 1L do begin
      i = w * n_uniq_pols + p
      correct = where(flat_waves[w] eq wavelengths and flat_pols[p] eq pols, count)
      if (count eq 0L) then begin
        mg_log, 'no correct_wave for %f in %s', flat_waves[w], flatfile, $
                name='comp', /warn
        continue
      endif

      mg_log, 'correct_wave: %s', strjoin(strtrim(correct, 2), ', '), $
              name='comp', /debug
      mn = min(dt[correct], good)
      iflat = correct[good] + 1

      fits_read, fcb, dat, flat_header, exten_no=iflat
      ; make sure there aren't any zeros
      bad = where(dat eq 0.0, count)
      if (count gt 0L) then begin
        mg_log, 'zeros in flat at pixels %s', strjoin(strtrim(bad, 2), ', '), $
                name='comp', /warn
      endif

      flat[*, *, i] = float(dat)
      flat_names[i] = sxpar(flat_header, 'FILENAME')

      exposure = exposures[iflat - 1L]
      mg_log, 'iflat = %d, flat_waves[iw] = %f, times[iflat - 1] = %f, ' $
              + ' wavelengths[iflat - 1] = %f, exposures[iflat - 1] = %f', $
              iflat, flat_waves[w], times[iflat - 1], wavelengths[iflat - 1], $
              exposures[iflat - 1], $
              name='comp', /debug
    endfor
  endfor

  fits_close, fcb
end
