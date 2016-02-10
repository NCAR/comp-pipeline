; docformat = 'rst'

;+
; Demodulate a set of CoMP raw images. That is, turn Stokes I + Q and I - Q
; into Stokes I and Q, etc.
;
; :Uses:
;   comp_inventory_header, comp_get_component, sxaddpar, sxpar
;
; :Params:
;   rawimages : in, required, type="float(nx, ny, n_images)"
;     an array of images to be demodulated
;   rawheaders : in, required, type="strarr(n_tags, n_images)"
;     the fits headers corresponding to `rawimages`
;   images : out, required, type="fltarr(nx, ny, n_waves*n_beams*np)"
;     the demodulated (Stokes I/Q/U/V) images
;   headers : out, required, type="strarr(n_tags+1, n_waves*n_beams*np)"
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

  uniq_polstates = polstates[uniq[polstates, sort(polstates)]]

  uniq_waves = waves[uniq[waves, sort(waves)]]
  n_waves = n_elements(uniq_waves)

  ; conversion factor from disk intensity to photons (for 250ms exposures)
  photfac = double(1.0 / sqrt(875.0))

  ; find which polarizations are in this file
  pol_labels = ['Q', 'U', 'V']
  np0 = n_elements(pol_labels)
  all_polarizations = [['I+' + pol_labels], $
                       ['I-' + pol_labels]]
  polstate_available = intarr(np0)
  for p = 0L, np0 - 1L do begin
    polstate_available[p] = (total(polstates eq all_polarizations[p, 0]) gt 0) $
              and (total(polstates eq all_polarizations[p, 1]) gt 0)
  endfor
  polstate_available_ind = where(polstate_available)
  np = total(polstate_available, /integer)

  ; .sav file defines cal_struct variable
  restore, filename=cal_file

  for w = 0L, n_elements(uniq_waves) - 1L do begin
    for b = 0L, n_elements(beams) - 1L do begin
      for p = 0L, n_elements(uniq_polstates) - 1L do begin
        ; get average for each pol/beam/wavelength state
        datai = comp_get_component(rawimages, rawheaders, $
                                   uniq_polstates[p], $
                                   beams[b], $
                                   uniq_waves[w], $
                                   headersout=headersout)
        ; TODO: divide by exposure time
        vars = photfac * abs(datai) * sxpar(headersout, 'NAVERAGE')
        ; TODO: compute NAVERAGE
        images_demod = comp_calibrate_stokes(datai, $
                                             vars, $
                                             pol, $
                                             cal_struct, $
                                             stokeslabels=stokeslabels)
      endfor
    endfor
  endfor

  ; np + 1 since 0th polarization is Stokes I
  ; one new tag (ntags + 1) for number of exposures in average
  headers = strarr(ntags + 1, (np + 1) * n_beams * n_waves)
  ; output images ordered by polarization-beam-wavelength (wavelength tightest)
  images = dblarr(nx, ny, (np + 1) * n_beams * n_waves)

  for i = 0L, np - 1L do begin
    for j = 0L, n_beams - 1L do begin
      ; pull out the plus and minus Stokes component at each wavelength
      ipstokes = comp_get_component(rawimages, rawheaders, ps_all[polstate_available_ind[i], 0], beams[j], headersout=ipheads)
      imstokes = comp_get_component(rawimages, rawheaders, ps_all[polstate_available_ind[i], 1], beams[j], headersout=imheads)

      ; form I and Q, U, or V and put them in the appropriate places in the
      ; image and header arrays:
      for k = 0L, n_waves - 1L do begin
        ; TODO: this code assumes NAVERAGE is the same for ipheads and imheads
        naverage = sxpar(ipheads[*, k], 'NAVERAGE') + sxpar(imheads[*, k], 'NAVERAGE')

        headertemp = ipheads[*, k]

        ; set Stokes Q/U/V headers and images
        sxaddpar, headertemp, 'POLSTATE', pol_labels[polstate_available_ind[i]]
        sxaddpar, headertemp, 'NAVERAGE', naverage
        headers[*, (i + 1) * n_beams * n_waves + j * n_waves + k] = headertemp
        images[*, *, (i + 1) * n_beams * n_waves + j * n_waves + k] = 0.5 * (ipstokes[*, *, k] - imstokes[*, *, k])

        ; update Stokes I headers and images
        sxaddpar, headertemp, 'POLSTATE', 'I'
        sxaddpar, headertemp, 'NAVERAGE', $
                  sxpar(headertemp, 'NAVERAGE') + sxpar(headers[*, j * n_waves + k], 'NAVERAGE')

        headers[*, j * n_waves + k] = headertemp
        images[*, *, j * n_waves + k] += 0.5 * (ipstokes[*, *, k] + imstokes[ *, *, k]) * naverage
      endfor
    endfor
  endfor

  for j = 0L, n_beams - 1L do begin
    for k = 0L, n_waves - 1L do begin
      images[*, *, j * n_waves + k] /= sxpar(headers[*, j * n_waves + k], 'NAVERAGE')
    endfor
  endfor
end