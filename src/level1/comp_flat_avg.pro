; docformat = 'rst'

;+
; Average flats by wavelength.
;
; :Uses:
;   comp_dark_interp, comp_fixrock, comp_fix_image, comp_demultiplex,
;   fits_read, fits_close, sxpar
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   time : in, required, type=float
;     time of observation
;   wave : in, required, type=fltarr
;     wavelengths
;   uniq_waves : in, required, type=fltarr
;     unique wavelengths present
;   exposure : out, required, type=float
;     set to a named variable to retrieve exposure (ms)
;   fcbin : in, required, type=structure
;     FITS Control Block as returned by `FITS_OPEN`
;   flats : out, required, type="fltarr(1024, 1024, nwaves)"
;     flat images averaged by wavelength
;   nd_filter : out, optional, type=integer
;     set to a named variable to retrieve the ND filter for the flats
;
; :Keywords:
;   error : out, optional, type=long
;     set to a named variable to retrieve the error status of the averaging
;
; :Author:
;   MLSO Software Team
;
; :History:
;   used temporary and compound assignment operators to save memory
;     Oct 3 2014  GdT
;   see git log for recent changes
;-
pro comp_flat_avg, date_dir, time, wave, uniq_waves, exposure, fcbin, flats, nd_filter, $
                   error=error
  compile_opt strictarr
  @comp_config_common

  error = 0L

  ; skip first image at each wavelength or just first image in recipe
  skip_first = flat_avg_skip_first

  nwaves = n_elements(uniq_waves)
  wave_type = comp_find_wave_type(wave, /name)

  ; do not use first image at each wavelength
  wave[0] = -1   ; do not use first image
  if (skip_first) then begin
    for i = 0L, nwaves - 1L do begin
      good = where(wave eq uniq_waves[i], count)
      if (count eq 0) then begin
        mg_log, 'no good wave in first loop', name='comp', /warn
        continue
      endif
      wave[good[0]] = -1
    endfor
  endif

  flats = fltarr(1024, 1024, nwaves)
  for i = 0L, nwaves - 1L do begin
    good = where(wave eq uniq_waves[i], count)
    if (count eq 0) then begin
      mg_log, 'no good wave in second loop', name='comp', /warn
      continue
    endif

    for j = 0L, count - 1L do begin
      fits_read, fcbin, dat, header, exten_no=good[j] + 1, /no_abort, message=message
      if (message ne '') then begin
        mg_log, 'error reading ext %d', good[j] + 1, name='comp', /error
        error = 1
        return
      endif

      ; log polarization state, wavelength, and beam for this file
      ext_polstate   = sxpar(header, 'POLSTATE')
      ext_beam       = sxpar(header, 'BEAM')
      ext_wavelength = sxpar(header, 'WAVELENG')
      mg_log, '%d/%d: ext %d, %s, beam %d, %0.2f', $
              j + 1, count, good[j] + 1, ext_polstate, ext_beam, ext_wavelength, $
              name='comp', /debug

      exposure = sxpar(header, 'EXPOSURE')
      nd_filter = comp_get_nd_filter(date_dir, wave_type, header, error=nd_error)
      if (nd_error ne 0) then begin
        mg_log, 'error reading NDFILTER, using 8', name='comp', /warn
      endif

      if (sxpar(header, 'DEMULT') eq 0) then dat = comp_demultiplex(temporary(dat))
      dat = float(dat)
      dat -= comp_dark_interp(date_dir, time, exposure)
      dat = comp_fixrock(temporary(dat), 0.03)
      dat = comp_fix_image(temporary(dat))
      flats[*, *, i] += dat
    endfor
    flats[*, *, i] = temporary(flats[*, *, i]) / float(count)
  endfor
  fits_close, fcbin
end
