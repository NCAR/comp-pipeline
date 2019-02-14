; docformat = 'rst'

;+
; Lookup continuum correction given a date/time and wave type, i.e., '1074' or
; '1079'.
;
; :Params:
;   date : in, required, type=string
;     date in the form 'YYYYMMDD'
;   time : in, required, type=string
;     time in the form 'HHMMSS' in HST
;   wave_type : in, required, type=string
;     wave type, i.e., '1074' or '1079'
;
; :Keywords:
;   found : out, optional, type=boolean
;     set to a named variable to retrieve whether a matching continuum
;     correction was found for the given date/time and wave type
;   correction_factors : out, optional, type=float
;     set to a named variable to retrieve the continuum correction
;   offset : out, optional, type="fltarr(n_flats, n_beams)"
;     set to a named variable to retrieve the wavelength offsets
;   flat_times : out, optional, type=fltarr(n_flats)
;     set to a named variable to retrieve the times of the flats in decimal
;     hours
;   n_flats : out, optional, type=long
;     set to a named variable to retrieve the number of 11 pt flats for the
;     given central wavelength
;   chisq : out, optional, type="fltarr(n_flats, n_beams)"
;     set to a named variable to retrieve the chi squared, all 0's in the
;     this case of lookup
;   wavelengths : out, optional, type="fltarr(n_flats, n_wavelengths)"
;     set to a named variable to treive the wavelengths for each 11 pt flat, the
;     standard list for this lookup
;-
pro comp_lookup_continuum_correction, date, wave_type, found=found, $
                                      correction_factors=correction_factors, $
                                      offset=offset, $
                                      n_flats=n_flats, $
                                      flat_times=flat_times, $
                                      chisq=chisq, $
                                      wavelengths=wavelengths
  compile_opt strictarr
  on_error, 2

  case wave_type of
    '1074': basename = 'wave_cal_1074_2.txt'
    '1079': basename = 'wave_cal_1079.txt'
    else: message, string(wave_type, format='(%"unknown wave type: %s")')
  endcase

  filename = filepath(basename, $
                      subdir=['../../resource'])
                      root=mg_src_root())

  n_lines = file_lines(filename)
  openr, lun, filename, /get_lun
  lines = strarr(n_lines)
  readf, lun, lines
  free_lun, lun

  dates = strmid(lines, 0, 8)
  rest  = strmid(lines, 8)
  data  = fltarr(4, n_lines)
  reads, rest, data

  found = 0B
  matching_dates_indices = where(dates eq date, n_matching_dates)
  if (n_matching_dates eq 0L) then return
  found = 1B

  ; there are two entries (on band and off band) for each set of 11 pt flats
  n_flats = n_matching_dates / 2L

  ; default 11 pt flat wavelengths
  n_wavelengths = 11L
  case wave_type of
    '1074': wavelengths = [1074.02, 1074.14, 1074.26, 1074.38, 1074.50, 1074.62, $
                           1074.74, 1074.86, 1074.98, 1075.10, 1075.22]
    '1079': wavelengths = [1079.18, 1079.30, 1079.42, 1079.54, 1079.66, 1079.78, $
                           1079.90, 1080.02, 1080.14, 1080.26, 1080.38]
  endcase
  wavelengths = rebin(reform(wavelengths, 1, n_wavelengths), n_flats, n_wavelengths)
  
  ; default chi squared values
  chisq = fltarr(n_flats, 2)

  ; sets of 11 pt flats come in pairs (on band/off band), so just take first in
  ; each pair
  flat_times = data[0, matching_dates_indices[0:*:2]]

  ; offset is 3rd column of original data
  offset = reform(data[1, matching_dates_indices], n_flats, 2)

  case wave_type of
    '1074': lam0 = 1074.7
    '1079': lam0 = 1079.8
  endcase

  comp_get_spectrum_solar_telluric, lam0, lambda, solar_spec, telluric_spec

  n_lambda = n_elements(lambda)
  d_lambda = lambda[1] - lambda[0]

  solar_spec = solar_spec / max(solar_spec)
  telluric_spec = telluric_spec / max(telluric_spec)

  correction_factors = dblarr(n_flats, n_wavelengths)

  for f = 0L, n_flats - 1L do begin
    shift_sol = interpolate(solar_spec, $
                            dindgen(n_lambda) + offset[f, 0] / d_lambda, $
                            missing=1.0, $
                            /double)
    shift_tell = interpolate(telluric_spec, $
                             dindgen(n_lambda) + offset[f, 0] / d_lambda, $
                             missing=1.0, $
                             /double)
    ; TODO: p1[1] = ?, is it data[2, 2 * f]?
    shift_tell = (1.0 - (1.0 - shift_tell) * p1[1]) > 0.0

    for w = 0L, n_wavelengths - 1L do begin
      comp_trans, wavelengths[w], lambda, trans_on, trans_off
      trans_on = trans_on / total(trans_on)
      ; TODO: p1[2] = ?, is it 1.0?
      correction_factors[f, w]  = p1[2] * total(trans_on[*, w] * shift_sol * shift_tell)
    endfor
  endfor
end
