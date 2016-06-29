; docformat = 'rst'

;+
; Combine the foreground and background (continuum) beams of CoMP, returning
; background-subtracted foreground and the background.
;
; :Uses:
;   comp_inventory_header, comp_get_component, comp_image_geometry,
;   comp_extract_beams, sxpar, sxaddpar, sxdelpar
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
; :Keywords:
;   n_uniq_polstates : out, optional, type=long
;     set to a named variable to retrieve the number of unique
;     polarization states
;   n_uniq_wavelengths : out, optional, type=long
;     set to a named variable to retrieve the number of unique wavelengths
;   image_geometry : out, required, type=structure
;     image geometry specifications
;   wave_type : in, required, type=string
;     wavelength to process, '1074', '1079', etc.
;
; :Author:
;   Joseph Plowman
;-
pro comp_combine_beams, images, headers, date_dir, $
                        images_combine, headers_combine, $
                        n_uniq_polstates=np, n_uniq_wavelengths=nw, $
                        image_geometry=plus_image_geometry, $
                        wave_type=wave_type
  compile_opt strictarr
  @comp_constants_common

  comp_inventory_header, headers, beam, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret

  uwave = wave[uniq(wave, sort(wave))]  ; unique waves
  upol = pol[uniq(pol, sort(pol))]      ; unique polarizations
  nw = n_elements(uwave)
  np = n_elements(upol)

  ntags = n_elements(headers[*, 0])
  if (sxpar(headers[*, 0], 'BEAM') ne 0) then ntags--

  ; output image and header array
  images_combine = dblarr(nx, ny, 2 * np * nw)
  headers_combine = strarr(ntags, 2 * np * nw)

  ; call comp_image_geometry
  plus_indices = where(beam gt 0, n_plus_images, $
                       complement=minus_indices, $
                       ncomplement=n_minus_images)

  plus_images = images[*, *, plus_indices]
  plus_headers = headers[*, plus_indices]
  minus_images = images[*, *, minus_indices]
  minus_headers = headers[*, minus_indices]

  plus_image_geometry = comp_image_geometry(plus_images, plus_headers, date_dir)
  minus_image_geometry = comp_image_geometry(minus_images, minus_headers, date_dir)

  for i = 0L, np - 1L do begin   ; loop over unique polarizations
    for j = 0L, nw - 1L do begin   ; loop over unique wavelengths
      ; get the two beam states for this wavelength and polarization
      imgplus = comp_get_component(images, headers, upol[i], 1, uwave[j], $
                                   headersout=hplus, /noskip)
      imgminus = comp_get_component(images, headers, upol[i], -1, uwave[j], $
                                    headersout=hminus, /noskip)

      ; extract the foreground and background subimages from both
      comp_extract_beams, imgplus, hplus, date_dir, bgplus, fgplus, $
                          image_geometry=plus_image_geometry
      comp_extract_beams, imgminus, hminus, date_dir, fgminus, bgminus, $
                          image_geometry=minus_image_geometry

      ; foreground part (with background subtracted); note: the He background
      ; is contaminated, so don't subtract
      if (wave_type eq '1083') then begin
        images_combine[*, *, i * nw + j] = 0.5 * (fgplus + fgminus)
      endif else begin
        images_combine[*, *, i * nw + j] = 0.5 * (fgplus - bgminus + fgminus - bgplus)
      endelse

      ; background part
      images_combine[*, *, np * nw + i * nw + j] = 0.5 * (bgminus + bgplus)

      ; update the headers
      sxdelpar, hplus, 'BEAM'
      sxaddpar, hplus, 'NAVERAGE', $
                sxpar(hplus, 'NAVERAGE') + sxpar(hminus, 'NAVERAGE')
      headers_combine[*, i * nw + j] = hplus
      sxaddpar, hplus, 'POLSTATE', 'BKG' + upol[i]
      headers_combine[*, np * nw + i * nw + j] = hplus
    endfor
  endfor
end
