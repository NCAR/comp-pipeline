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

  ; set some constants from the inputs
  nx = n_elements(rawimages[*, 0, 0])
  ny = n_elements(rawimages[0, *, 0])
  ntags = n_elements(rawheaders[*, 0])

  comp_inventory_header, rawheaders, beams, groups, waves, polstates, type, $
                         expose, cover, cal_pol, cal_ret

  ; always 2 beams for CoMP: -1 and +1
  beams = [-1, 1]
  n_beams = 2

  uniq_waves = waves[uniq[waves, sort(waves)]]
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

  for p = 0L, n_elements(n_polstates) - 1L do begin
    for b = 0L, n_elements(beams) - 1L do begin
      for w = 0L, n_elements(uniq_waves) - 1L do begin
        pluspol = all_polarizations[polstates_available_ind[p], 0]
        minuspol = all_polarizations[polstates_available_ind[p], 1]

        ; get average for each pol/beam/wavelength state
        pluspol_datai = comp_get_component(rawimages, rawheaders, $
                                           pluspol, $
                                           beams[b], $
                                           uniq_waves[w], $
                                           headersout=pluspol_headers)
        minus_dataipol = comp_get_component(rawimages, rawheaders, $
                                            minuspol, $
                                            beams[b], $
                                            uniq_waves[w], $
                                            headersout=minuspol_headers)

        ; TODO: divide by exposure time

        pluspol_vars = photfac * abs(pluspol_datai) * sxpar(pluspol_headers, 'NAVERAGE')
        minuspol_vars = photfac * abs(minuspol_datai) * sxpar(minuspol_headers, 'NAVERAGE')

        pluspol_images = comp_calibrate_stokes(pluspol_datai, $
                                               pluspol_vars, $
                                               pluspol, $
                                               cal_struct, $
                                               stokeslabels=stokeslabels)
        minuspol_images = comp_calibrate_stokes(minuspol_datai, $
                                                minuspol_vars, $
                                                minuspol, $
                                                cal_struct, $
                                                stokeslabels=stokeslabels)
        ; compute NAVERAGE
        ; TODO: this code assumes NAVERAGE is the same for pluspol_headers and
        ; minuspol_headers
        naverage = sxpar(pluspol_headers, 'NAVERAGE') + sxpar(minuspol_headers, 'NAVERAGE')

        ; update images and headers

        temp_headers = pluspol_headers

        ; set Stokes Q/U/V headers and images
        sxaddpar, temp_headers, 'POLSTATE', pol_labels[polstate_available_ind[i]]
        sxaddpar, temp_headers, 'NAVERAGE', naverage
        headers[*, (p + 1) * n_beams * n_waves + b * n_waves + w] = temp_headers
        images[*, *, (p + 1) * n_beams * n_waves + b * n_waves + w] $
          = 0.5 * (pluspol_images[*, *, k] - minuspol_images[*, *, w])

        ; update Stokes I headers and images
        sxaddpar, temp_headers, 'POLSTATE', 'I'
        sxaddpar, temp_headers, 'NAVERAGE', $
                  sxpar(temp_headers, 'NAVERAGE') $
                    + sxpar(headers[*, b * n_waves + w], 'NAVERAGE')

        headers[*, b * n_waves + w] = temp_headers
        images[*, *, b * n_waves + w] $
          += 0.5 * (pluspol_images[*, *, w] + minuspol_images[ *, *, w]) * naverage

      endfor
    endfor
  endfor

  for b = 0L, n_beams - 1L do begin
    for w = 0L, n_waves - 1L do begin
      images[*, *, b * n_waves + w] /= sxpar(headers[*, b * n_waves + w], 'NAVERAGE')
    endfor
  endfor
end