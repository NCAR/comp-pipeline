; docformat = 'rst'

;+
; Average images with the same wavelength, beam state, and polarization state in
; a particular file of flats.
;
; :Uses:
;   comp_dark_interp, comp_fixrock, comp_fix_image, comp_demultiplex,
;   fits_read, fits_close, sxpar
;
; :History:
;   used temporary and compound assignment operators to save memory
;     Oct 3 2014  GdT
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   time : in, required, type=float
;     time of day as a decimal day
;   wave : in, required, type=fltarr
;     array of wavelengths (possibly multiplied by beam state) in file
;   uniq_waves : in required, type=fltarr
;     array of unique wavelength-beam state values
;   pol : in, required, type=strarr
;     array of polarization states available in file
;   uniq_pols : in, required, type=strarr
;     array of unique polarization states
;   exposure : out, required, type=float
;     set to a named variable to retrieve the exposure value for the flats in
;     the file
;   fcbin : in, required, type=structure
;     FITS identifier for current flat file
;   flats : out, optional, type="fltarr(1024, 1024, nflats)"
;     set to a named variable to retrieve the flats averaged by wavelength-beam
;     and polarization state for the file
;-
pro comp_flat_avg, date_dir, time, wave, uniq_waves, pol, uniq_pols, exposure, fcbin, flats
  compile_opt strictarr
  @comp_config_common

  ; skip first image at each wavelength or just first image in recipe
  skip_first = flat_avg_skip_first

  n_uniq_waves = n_elements(uniq_waves)
  n_uniq_pols = n_elements(uniq_pols)

  ; do not use first image at each wavelength
  wave[0] = -1   ; do not use first image
  if (skip_first) then begin
    for i = 0L, n_uniq_waves - 1L do begin
      good = where(wave eq uniq_waves[i], count)
      if (count eq 0) then begin
        mg_log, 'no good wave in first loop', name='comp', /warn
        continue
      endif
      wave[good[0]] = -1
    endfor
  endif

  flats = fltarr(1024, 1024, n_uniq_waves * n_uniq_pols)
  for w = 0L, n_uniq_waves - 1L do begin
    for p = 0L, n_uniq_pols - 1L do begin
      i = w * n_uniq_pols + p
      good = where(wave eq uniq_waves[w] and pol eq uniq_pols[p], count)
      if (count eq 0) then begin
        mg_log, 'no good wave in second loop', name='comp', /warn
        continue
      endif

      for j = 0L, count - 1L do begin
        fits_read, fcbin, dat, header, exten_no=good[j] + 1
        exposure = sxpar(header, 'EXPOSURE')

        if (sxpar(header, 'DEMULT') eq 0) then dat = comp_demultiplex(temporary(dat))
        dat = float(dat)
        dat -= comp_dark_interp(date_dir, time, exposure)
        dat = comp_fixrock(temporary(dat), 0.03)
        dat = comp_fix_image(temporary(dat))
        flats[*, *, i] += dat
      endfor
      flats[*, *, i] = temporary(flats[*, *, i]) / float(count)
    endfor
  endfor
  fits_close, fcbin
end
