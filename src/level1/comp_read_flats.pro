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
;   wave
;     array of wavelengths of data images
;   beam
;     array of beam values for data images
;   time
;     time of data image
;   flat : out, required, type="fltarr(1024, 1024, nwave)"
;   flat_header : out, required, type=strarr
;   flat_waves : out, optional, type=fltarr
;     set to retrive the unique wavelengths in `wave`, possibly given sign
;     through multiplying by beam state
;   flat_exposure : out, optional, type=fltarr
;     set to retrieve the exposure times for the returned flats
;
; :Keywords:
;   file : in, optional, type=string, default='flat.fts'
;     specify filename of flat file to read
;   flat_extensions : out, optional, type=lonarr(nwave)
;     set to a named variable to retrieve the extension of the flat file used
;
; :Author:
;   Tomczyk
;-
pro comp_read_flats, date_dir, wave, beam, time, flat, flat_header, $
                     flat_waves, flat_names, flat_exposure, $
                     file=file, flat_extensions=flat_extensions
  compile_opt idl2
  @comp_constants_common
  @comp_config_common
  @comp_flats_common

  process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)

  ; multiply beam times wave to get unique waves
  beam_multiplies_wave = read_flats_beam_multiplies_wave

  if (beam_multiplies_wave) then begin
    wb = wave * beam
  endif else begin
    wb = wave
  endelse

  flat_waves = wb[comp_uniq(wb, sort(wb))]
  nwave = n_elements(flat_waves)
  mg_log, 'wavelengths: %s', strjoin(strtrim(flat_waves, 2), ', '), $
          name='comp', /debug

  flat = fltarr(1024, 1024, nwave, /nozero)
  flat_names = strarr(nwave)
  flat_extensions = lonarr(nwave)
  flat_exposure = fltarr(nwave)

  ; flat field filename
  if (keyword_set(file)) then begin
    flatfile = filepath(file, root=process_dir)
  endif else begin
    flatfile = filepath('flat.fts', root=process_dir)
  endelse

  if (~cache_flats) then begin
    fits_open, flatfile, fcb
    num = fcb.nextend

    ; read arrays with times, wavelengths and polarizations
    fits_read, fcb, flat_times, exten_no=num - 2
    fits_read, fcb, flat_wavelengths, exten_no=num - 1
    fits_read, fcb, flat_exposures, exten_no=num
  endif

  dt = time - flat_times   ; find time difference from flat times
  bad = where(dt lt 0., count)
  if (count gt 0L) then dt[bad] = 1000.  ; use only flats before time

  ; use closest flat in time with correct wavelength and polarization state
  for iw = 0L, nwave - 1L do begin
    correct_wave = where(flat_waves[iw] eq flat_wavelengths, count)
    if (count eq 0L) then begin
      mg_log, 'no correct_wave for %f in %s', flat_waves[iw], flatfile, $
              name='comp', /warn
      continue
    endif

    mg_log, 'extensions for %0.2f: %s', $
            flat_waves[iw], $
            strjoin(strtrim(correct_wave + 1, 2), ', '), $
            name='comp', /debug
    mn = min(dt[correct_wave], good)
    iflat = correct_wave[good] + 1   ; FITS extensions start at 1
    flat_extensions[iw] = iflat

    if (cache_flats) then begin
      image = flat_images[*, *, iflat - 1]
      flat_header = flat_headers[*, iflat - 1]
    endif else begin
      fits_read, fcb, image, flat_header, exten_no=iflat
    endelse

    ; make sure there aren't any zeros
    bad = where(image eq 0.0, count)
    if (count gt 0L) then begin
      mg_log, 'zeros in flat at pixels %s', strjoin(strtrim(bad, 2), ', '), $
              name='comp', /warn
    endif

    if (make_flat_fill) then begin
      mask_full_fill = comp_annulus_1024(flat_header, o_offset=0.0, f_offset=0.0)
      good_pixels = where(mask_full_fill eq 1.0, n_good_pixels, $
                          complement=bad_pixels, ncomplement=n_bad_pixels)
      medflat = median(image[good_pixels])
      image[bad_pixels] = medflat
      mg_log, 'filling flat values with %0.2f outside annulus', medflat, $
              name='comp', /debug
    endif

    flat[*, *, iw] = float(image)
    flat_names[iw] = sxpar(flat_header, 'FILENAME')

    flat_exposure[iw] = flat_exposures[iflat - 1L]
    mg_log, 'closest flat ext %d:', iflat, $
            name='comp', /debug
    mg_log, '  time=%s, wave=%0.2f, exposure=%0.1fms', $
            comp_times2str(flat_times[iflat - 1]), $
            flat_wavelengths[iflat - 1], $
            flat_exposures[iflat - 1], $
            name='comp', /debug
  endfor

  if (~cache_flats) then begin
    fits_close, fcb
  endif
end
