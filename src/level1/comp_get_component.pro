; docformat = 'rst'

;+
; Retrieves a specified CoMP component (i.e., polarization state and FG/BG beam
; setting) from a set of images and headers.
;
; :Uses:
;   comp_inventory_header, sxpar, sxaddpar
;
; :Returns:
;   array of images at the specified polarization states, beam settings, and
;   (optionally) wavelengths
;
; :Params:
;   images : in, required, type="fltarr(nx, ny, nimg)"
;     the array of images from which to retrieve the desired component
;   headers : in, required, type="strarr(ntag, nimg)"
;     the header array corresponding to images
;   polstate : in, required, type=string/strarr
;     the desired polarization state, e.g., 'I+V', 'Q', 'BKGI'
;   beam : in, required, type=integer
;     the desired beam setting (+1 or -1); if this data has beams combined
;     (level 1 or greater), set beam to 0
;   wave : in, optional, type=float/fltarr
;     the desired wavelength(s); if undefined, all wavelengths will be
;     returned; if wave is passed in but undefined, it will contain the
;     wavelengths on return
;
; :Keywords:
;   skipall : in, optional, type=boolean
;     skip the first image at every wavelength
;   count : out, optional, type=long
;     the number of images averaged at each wavelength
;   headersout : out, optional, type="strarr(varies, nimg)"
;     an updated set of headers (adds or updates the 'NAVERAGE' flag)
;   average_wavelengths : in, optional, type=boolean
;     average over wavelengths; don't set if `polstate` is an array
;   n_wavelengths : in, optional, type=integer, default=all
;     number of wavelengths to average over if `AVERAGE_WAVELENGTHS`
;     is set
;   noskip : in, optional, type=boolean
;     don't skip the very first image (due to an instrument issue, the very
;     first image in each raw file is bad)
;
; :Author:
;   Joseph Plowman
;-
function comp_get_component, images, headers, polstate, beam, wave, $
                             skipall=skipall, $
                             count=count, $
                             headersout=headersout, $
                             average_wavelengths=average_wavelengths, $
                             n_wavelengths=n_wavelengths, $
                             noskip=noskip
  compile_opt strictarr
  on_error, 2

  ; figure out what's in this image array
  comp_inventory_header, headers, beams, groups, waves, polstates, type, $
                         expose, cover, cal_pol, cal_ret

  ; if we don't have an input list of wavelengths, use all of them
  if (n_elements(wave) eq 0L) then wave = waves[uniq(waves, sort(waves))]
  nw = n_elements(wave)
  ntags = n_elements(headers[*, 0L])

  ; if this header is missing NAVERAGE (e.g., it's a raw file), the output
  ; header will need another tag
  if (sxpar(headers[*, 0L], 'NAVERAGE') eq 0L) then ntags++

  ; flags to check if polarization states and beams match...
  beam_check = beams eq beam

  ; skip very first image, which is bad due to instrument issue...
  if (keyword_set(skipall) eq 0 and keyword_set(noskip) eq 0) then beam_check[0] = 0

  count = lonarr(nw, n_elements(polstate))
  dims = size(images, /dimensions)
  imgout = make_array(dimension=[dims[0:1], nw, n_elements(polstate)], $
                      type=size(image, /type))
  headersout = strarr(ntags, nw, n_elements(polstate))

  ; loop over unique wavelengths...
  for w = 0L, nw - 1L do begin
    for p = 0L, n_elements(polstate) - 1L do begin
      ; find which indices have matching wavelength, polstate, and beam...
      checkwp = where(beam_check and polstate[p] eq polstates and waves eq wave[w], $
                      countwp)
      imagewp = images[*, *, checkwp]
      if (countwp lt 1) then begin
        message, 'no image at specified polarization/beam/wave'
      endif
      if (keyword_set(skipall)) then begin
        imagewp = imagewp[*, *, 1:countwp - 1]   ; skip first image at all wavelengths...
        --countwp
      endif

      ; average over images with same wavelengths, polstate, and beam:
      if (countwp gt 1) then imagewp = mean(imagewp, dimension=3)
      imgout[*, *, w, p] = imagewp

      ; update headers...
      if (sxpar(headers[*, 0L], 'NAVERAGE') eq 0) then begin
        count[w, p] = countwp
      endif else begin
        for j = 0L, n_elements(checkwp) - 1L do begin
          count[w, p] += sxpar(headers[*, checki[j]], 'NAVERAGE')
        endfor
      endelse
      headertemp = headers[*, checkwp[0]]
      sxaddpar, headertemp, 'NAVERAGE', count[w, p], $
                ' Number of images averaged together', $
                after='EXPOSURE'
      headersout[*, w, p] = headertemp
    endfor
  endfor

  ; average over all wavelengths if AVERAGE_WAVELENGTHS is set
  if (keyword_set(average_wavelengths) and nw gt 1L) then begin
    if (n_elements(polstate) gt 1L) then begin
      message, 'AVERAGE_WAVELENGTHS cannot be set if polstate is an array'
    endif
    if (n_elements(n_wavelengths) eq 0L) then begin
      start_index = 0L
      end_index = nw - 1L
    endif else begin
      start_index = (nw - n_wavelengths) / 2 > 0
      end_index = (start_index + n_wavelengths - 1L) < (nw - 1L)
    endelse
    imgout = mean(imgout[*, *, start_index:end_index], dimension=3L)
    count = total(count)
    headersout = headersout[*, 0, 0]
    sxaddpar, headersout, 'NAVERAGE', count, $
              ' Number of images averaged together', $
              after='EXPOSURE'
  endif

  headersout = reform(headersout)

  return, reform(imgout)
end
