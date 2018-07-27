; docformat = 'rst'

;+
; Demodulate a set of CoMP raw images. That is, turn Stokes I + Q and I - Q
; into Stokes I and Q, etc.
;
; :Uses:
;   comp_inventory_header, comp_get_component, sxaddpar, sxpar
;
; :Params:
;   rawimages : in, required, type = real array (nx*ny*nimg)
;     An array of images to be demodulated.
;   rawheaders : in, required, type = string array (ntags*nimg)
;     The fits headers corresponding to rawimages.
;   images : out, required, type = real array (nx by ny by (nw*nb*np))
;     The demodulated (Stokes I/Q/U/V) images.
;   headers : out, required, type = string array (ntags+1 by (nw*nb*np))
;     Headers corresponding to images.
;
; :Author:
;   MLSO Software Team
;-
pro comp_demodulate, rawimages, rawheaders, images, headers
  compile_opt strictarr

  comp_inventory_header, rawheaders, beams, waves, polstates, type, $
                         expose, cover, cal_pol, cal_ret

  beams = [-1, 1]
  nb = 2   ; always 2 beams for CoMP
  nw = n_elements(uniq(waves, sort(waves)))

  pols = ['Q', 'U', 'V']   ; polarization labels
  np0 = n_elements(pols)   ; The number of possible polarizations
  ; strings for each possible polarization state (I+Q, I-Q, etc)
  ps_all = [['I+' + pols], ['I-' + pols]]
  pc = intarr(np0)   ; find which polarizations are in this file
  for i = 0L, np0 - 1L do begin
    pc[i] = (total(polstates eq ps_all[i, 0]) gt 0) and (total(polstates eq ps_all[i, 1]) gt 0)
  endfor
  wpc = where(pc)
  np = total(pc)

  nx = n_elements(rawimages[*, 0, 0])
  ny = n_elements(rawimages[0, *, 0])
  ntags = n_elements(rawheaders[*, 0])

  ; output images are ordered polarization->beam->wavelength (wavelength tightest)
  ; one new tag for number of exposures in average, i.e., NAVERAGE
  headers = strarr(ntags + 1, (np + 1) * nw * nb)
  images = dblarr(nx, ny, (np + 1) * nw * nb)  ; np+1 since 0th polarization is Stokes I

  for i = 0L, np - 1L do begin
    for j = 0L, nb - 1L do begin
      ; pull out the plus and minus Stokes component at each wavelength
      ipstokes = comp_get_component(rawimages, rawheaders, ps_all[wpc[i], 0], $
                                    beams[j], headersout=ipheads)
      imstokes = comp_get_component(rawimages, rawheaders, ps_all[wpc[i], 1], $
                                    beams[j], headersout=imheads)

      ; form I and Q, U, or V and put them in the appropriate places in the
      ; image and header arrays:
      for k = 0L, nw - 1L do begin
        ; TODO: this code assumes NAVERAGE is the same for ipheads and impheads
        naverage = sxpar(ipheads[*, k], 'NAVERAGE') + sxpar(imheads[*, k], 'NAVERAGE')

        headertemp = comp_combine_headers([[ipheads[*, k]], [imheads[*, k]]], [0, 1])

        ; set Stokes Q/U/V headers and images
        sxaddpar, headertemp, 'POLSTATE', pols[wpc[i]]
        sxaddpar, headertemp, 'NAVERAGE', naverage

        headers[*, (i + 1) * nb * nw + j * nw + k] = headertemp
        images[*, *, (i + 1) * nb * nw + j * nw + k] = 0.5 * (ipstokes[*, *, k] - imstokes[*, *, k])

        ; update Stokes I headers and images
        sxaddpar, headertemp, 'POLSTATE', 'I'
        sxaddpar, headertemp, 'NAVERAGE', $
                  sxpar(headertemp, 'NAVERAGE') + sxpar(headers[*, j * nw + k], 'NAVERAGE')

        headers[*, j * nw + k] = headertemp
        images[*, *, j * nw + k] += 0.5 * (ipstokes[*, *, k] $
                                             + imstokes[ *, *, k]) * naverage
      endfor
    endfor
  endfor

  for j = 0L, nb - 1L do begin
    for k = 0L, nw - 1L do begin
      images[*, *, j * nw + k] /= sxpar(headers[*, j * nw + k], 'NAVERAGE')
    endfor
  endfor
end
