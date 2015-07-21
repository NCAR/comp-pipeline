; docformat = 'rst'

;+
; Combine the foreground and background (continuum) beams of CoMP, returning
; background-subtracted foreground and the background.
;
; :Uses:
;   comp_inventory_header, comp_get_component, comp_extract_beams, sxpar,
;   sxaddpar, sxdelpar
;
; :Params:
;   images : in, required, type="fltarr(nx, ny, nimg)"
;     the array of images whos beams are to be combined
;   headers : in, required, type="strarr(ntags, nimg)"
;     headers corresponding to images
;   date_dir : in, required, type=string
;     the date directory for this data
;   images_combine : out, required, type="fltarr(620, 620, (2*np*nw))"
;     the combined images, one foreground, one background for each wavelength
;     and stokes component. The last index is sorted first by wavelength
;     (successive wavelengths are adjacent), then by polarization
;   headers_combine : out, optional, type="strarr(ntags2, (2*np*nw))"
;     headers corresponding to `images_combine`; the 'BEAM' tag, if present,
;     will be removed from the header; `ntags2` will therefore generally be
;     equal to `ntags - 1`
;
; :Author:
;   Joseph Plowman
;-
pro comp_combine_beams, images, headers, date_dir, $
                        images_combine, headers_combine
  comppile_opt strictarr

  comp_inventory_header, headers, beam, group, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret

  uwave = wave(uniq(wave, sort(wave)))  ; unique waves
  upol = pol(uniq(pol, sort(pol)))      ; unique polarizations
  nw = n_elements(uwave)
  np = n_elements(upol)

  ntags = n_elements(headers[*, 0])
  if (sxpar(headers[*, 0], 'BEAM') ne 0) then ntags--

  nx = 620   ; these dimensions are typically hardwired in the CoMP pipeline
  ny = 620

  ; output image and header array
  images_combine = dblarr(nx, ny, 2 * np * nw)
  headers_combine = strarr(ntags, 2 * np * nw)

  for i = 0L, np - 1L do begin   ; loop over unique polarizations
    for j = 0L, nw - 1L do begin   ; loop over unique wavelengths
      ; get the two beam states for this wavelength and polarization
      imgplus = comp_get_component(images, headers, upol[i], 1, uwave[j], $
                                   headersout=hplus, /noskip)
      imgminus = comp_get_component(images, headers, upol[i], -1, uwave[j], $
                                    headersout=hminus, /noskip)

      ; extract the foreground and background subimages from both
      comp_extract_beams, imgplus, hplus, date_dir, bgplus, fgplus
      comp_extract_beams, imgminus, hminus, date_dir, fgminus, bgminus

      ; foreground part (with background subtracted)
      images_combine[*, *, i * nw + j] = 0.5 * (fgplus - bgminus + fgminus - bgplus)
      ; background part
      images_combine[*, *, np * nw + i * nw + j] = 0.5 * (bgminus + bgplus)

      ; update the headers
      sxdelpar, hplus, 'BEAM'
      sxaddpar, hplus, 'NAVERAGE', $
                sxpar(hplus, 'NAVERAGE') + sxpar(hminus, 'NAVERAGE')
      headers_combine[*, i * nw + j] = hplus
      sxaddpar, hplus, 'POLSTATE', 'B' + upol[i]
      headers_combine[*, np * nw + i * nw + j] = hplus
    endfor
  endfor
end