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
;   polstate : in, required, type=string
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
;   count : out, optional, type=long
;     the number of images averaged at each wavelength
;   headersout : out, optional, type="strarr(varies, nimg)"
;     an updated set of headers (adds or updates the 'NAVERAGE' flag)
;   average_wavelengths : in, optional, type=boolean
;     average over wavelengths
;   n_wavelengths : in, optional, type=integer, default=all
;     number of wavelengths to average over if `AVERAGE_WAVELENGTHS`
;     is set
;   noskip : in, optional, type=boolean
;     don't skip the very first image (due to an instrument issue, the very
;     first image in each raw file is bad)
;
; :Author:
;   MLSO Software Team
;-
function comp_get_component, images, headers, polstate, beam, wave, $
                             count=count, $
                             headersout=headersout, $
                             average_wavelengths=average_wavelengths, $
                             n_wavelengths=n_wavelengths, $
                             noskip=noskip
  compile_opt strictarr
  on_error, 2

  ; figure out what's in this image array
  comp_inventory_header, headers, beams, waves, polstates, type, $
                         expose, cover, cal_pol, cal_ret

  ; if we don't have an input list of wavelengths, use all of them:
  if (n_elements(wave) eq 0L) then wave = waves[uniq(waves, sort(waves))]
  nw = n_elements(wave)
  ntags = n_elements(headers[*, 0L])

  ; if this header is missing NAVERAGE (e.g., it's a raw file), the output
  ; header will need another tag:
  if (sxpar(headers[*, 0L], 'NAVERAGE') eq 0L) then ntags++

  ; flags to check if polarization states and beams match...
  check1 = polstates eq polstate and beams eq beam

  ; skip very first image, which is bad due to instrument issue...
  if (~keyword_set(noskip)) then check1[0] = 0

  count = lonarr(nw)
  imgout = images[*, *, 0L:nw - 1L]
  headersout = strarr(ntags, nw)

  ; loop over unique wavelengths...
  for i = 0L, nw - 1L do begin
    ; find which indices have matching wavelength, polstate, and beam...
    checki = where(check1 and waves eq wave[i], counti)
    if (counti lt 1) then begin
      message, 'no image at specified polarization/beam/wave'
    endif

    imagei = images[*, *, checki]

    ; average over images with same wavelengths, polstate, and beam:
    if (counti gt 1) then imagei = mean(imagei, dimension=3)
    imgout[*, *, i] = imagei

    ; update headers...
    if (sxpar(headers[*, 0L], 'NAVERAGE') eq 0) then begin
      count[i] = counti
    endif else begin
      for j = 0L, n_elements(checki) - 1L do begin
        count[i] += sxpar(headers[*, checki[j]], 'NAVERAGE')
      endfor
    endelse
    headertemp = headers[*, checki[0]]
    sxaddpar, headertemp, 'NAVERAGE', count[i], $
              ' Number of images averaged together', $
              after='EXPOSURE'
    headersout[*, i] = headertemp
  endfor

  ; average over all wavelengths if AVERAGE_WAVELENGTHS is set
  if (keyword_set(average_wavelengths) and nw gt 1L) then begin
    if (n_elements(n_wavelengths) eq 0L) then begin
      start_index = 0L
      end_index = nw - 1L
    endif else begin
      start_index = (nw - n_wavelengths) / 2 > 0
      end_index = (start_index + n_wavelengths - 1L) < (nw - 1L)
    endelse
    imgout = mean(imgout[*, *, start_index:end_index], dimension=3L)
    count = total(count)
    headersout = headersout[*, 0L]
    sxaddpar, headersout, 'NAVERAGE', count, $
              ' Number of images averaged together', $
              after='EXPOSURE'
  endif

  return, imgout
end


; main-level example program

comp_initialize, '20180101'
basename = '20180101.163930.FTS'
filename = filepath(basename, root='/hao/mahidata1/Data/CoMP/raw/20180101')
comp_read_data, filename, images, headers, primary_header

im = comp_get_component(images, headers, 'I+U', -1)
help, im

end
