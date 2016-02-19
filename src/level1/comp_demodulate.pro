; docformat = 'rst'

;+
; Demodulate a set of CoMP raw images. That is, turn Stokes I + Q and I - Q
; into Stokes I and Q, etc.
;
; :Uses:
;   comp_inventory_header, comp_get_component, comp_calibrate_stokes,
;   sxaddpar, sxpar
;
; :Params:
;   rawimages : in, required, type="float(nx, ny, n_images)"
;     an array of images to be demodulated
;   rawheaders : in, required, type="strarr(n_tags, n_images)"
;     the fits headers corresponding to `rawimages`
;   images : out, required, type="fltarr(nx, ny, n_waves*n_beams*n_polstates)"
;     the demodulated (Stokes I/Q/U/V) images
;   headers : out, required, type="strarr(n_tags+1, n_waves*n_beams*n_polstates)"
;     headers corresponding to images
;
; :Author:
;   Joseph Plowman
;-
pro comp_demodulate, rawimages, rawheaders, images, headers
  compile_opt strictarr
  ; TODO: needed for saving temporary results
  @comp_config_common

  ; set some constants from the inputs
  nx = n_elements(rawimages[*, 0, 0])
  ny = n_elements(rawimages[0, *, 0])
  ntags = n_elements(rawheaders[*, 0])

  comp_inventory_header, rawheaders, beams, groups, waves, polstates, type, $
                         expose, cover, cal_pol, cal_ret

  ; always 2 beams for CoMP: -1 and +1
  beams = [-1, 1]
  n_beams = 2

  uniq_waves = waves[uniq(waves, sort(waves))]
  n_waves = n_elements(uniq_waves)

  ; conversion factor from disk intensity to photons (for 250ms exposures)
  photfac = double(1.0 / sqrt(875.0))

  ; find which polarizations are in this file
  pol_labels = ['Q', 'U', 'V']
  np0 = n_elements(pol_labels)
  all_polarizations = [['I+' + pol_labels], $
                       ['I-' + pol_labels]]
  polstates_available = intarr(np0)
  for p = 0L, np0 - 1L do begin
    polstates_available[p] = (total(polstates eq all_polarizations[p, 0]) gt 0) $
                               and (total(polstates eq all_polarizations[p, 1]) gt 0)
  endfor
  polstates_available_ind = where(polstates_available)
  n_polstates = total(polstates_available, /integer)

  ; .sav file defines cal_struct variable
  restore, filename=cal_file

  ; n_polstates + 1 since 0th polarization is Stokes I
  ; one new tag (ntags + 1) for number of exposures in average
  headers = strarr(ntags + 1, (n_polstates + 1) * n_beams * n_waves)
  ; output images ordered by polarization-beam-wavelength (wavelength tightest)
  images = dblarr(nx, ny, (n_polstates + 1) * n_beams * n_waves)

  for w = 0L, n_elements(uniq_waves) - 1L do begin
    for b = 0L, n_elements(beams) - 1L do begin
      if (total(polstates eq 'V') gt 0) then begin
        ; TODO: get nearest QU file (see COMP_FIX_VXTALK for how)
      endif

      if (total(polstates eq 'Q') gt 0 or total(pol eq 'U') gt 0) then begin
        ; TODO: remove Stokes V col/row (from COMP_CALIBRATE_STOKES
        ; to COMP_COMPUTE_COEF_IMAGES)
      endif

      pols = [all_polarizations[polstates_available_ind, 0], $
              all_polarizations[polstates_available_ind, 1]]

      ; get average for each beam/wavelength state
      ; TODO: divide by exposure time
      pols_data = comp_get_component(rawimages, rawheaders, $
                                     pols, $
                                     beams[b], $
                                     uniq_waves[w], $
                                     headersout=pols_headers)

      pols_vars = photfac * abs(pols_data) * sxpar(pols_headers, 'NAVERAGE')

      pols_images = comp_calibrate_stokes(pols_data, $
                                          pols_vars, $
                                          pols, $
                                          cal_struct, $
                                          coef_images=coef_images, $
                                          stokeslabels=stokeslabels)

      ; save coef_images and pols_images (equation #32)
      save, coef_images, $
            filename=filepath(string(filename, uniq_waves[w], b, $
                                     format='(%"%s-coef-%s-%d.sav")'), $
                              root=process_basedir)
      save, pols_images, $
            filename=filepath(string(filename, uniq_waves[w], b, $
                                     format='(%"%s-pols-%s-%d.sav")'), $
                              root=process_basedir)


      ; update images and headers
      for p = 0L, n_polstates - 1L do begin
        ; compute NAVERAGE
        ; TODO: how to do this?
        ;naverage =

        ; set Stokes I/Q/U/V headers and images
        sxaddpar, pols_headers, 'POLSTATE', stokeslabels[p]
        sxaddpar, pols_headers, 'NAVERAGE', naverage
        headers[*, (p + 1) * n_beams * n_waves + b * n_waves + w] = pols_headers
        images[*, *, (p + 1) * n_beams * n_waves + b * n_waves + w] = pols_images[*, *, p]
      endfor
    endfor
  endfor
end


; main-level program

; run COMP_DEMODULATE on calibration code

@comp_config_common

; set some configuration variables
basename = '20150729.105218.FTS'
date_dir = '20150729'
config_filename = filepath('comp.mgalloy.compdata.calibration.cfg', $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())

; initialize
comp_initialize, date_dir

; configure with calibration config file
comp_configuration, config_filename=config_filename
;comp_setup_loggers
;comp_setup_loggers_date, date_dir

comp_file_type, date_dir

; apply darks/flats
if (~file_test(filepath('dark.fts', root=process_basedir))) then begin
  comp_make_dark, date_dir, error=error
endif
if (~file_test(filepath('flat.fts', root=process_basedir))) then begin
  comp_make_flat, date_dir, error=error
endif

filename = filepath(basename, subdir=date_dir, root=raw_basedir)
comp_read_data, filename, images, headers, header0
comp_apply_flats_darks, images, headers, date_dir

; call COMP_DEMODULATE
comp_demodulate, images, headers, images_demod, headers_demod

end
