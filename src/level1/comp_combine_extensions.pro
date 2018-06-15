; docformat = 'rst'

;+
; Combine like extensions, i.e., extensions that have the same polstate, beam,
; and wavelength. Combine the `images` and `headers` while also fixing up `pol`,
; `beam`, and `wave` for the new combination.
;
; :Params:
;   images : in, out, required, type="dblarr(xsize, ysize, nimages)"
;     the images from all the extensions in a file, on exit should be the
;     combined images of similar extensions
;   headers : in, out, required, type="strarr(ntags, nimages)"
;     headers for the corresponding images
;   pol : in, out, required, type=strarr(n_images)
;     polarization states of the corresponding images
;   beam : in, out, required, type=ntarr(n_images)
;     beam states of the corresponding images
;   wave : in, out, required, type=fltarr(n_images)
;     wavelengths of the corresponding images
;
; :Keywords:
;   error : out, optional, type=long
;     set to a named variable to retrieve the error state of the combination,
;     if non-zero, there was an error in combining the like extensions and the
;     result should not be used
;-
pro comp_combine_extensions, images, headers, pol, beam, wave, error=error
  compile_opt strictarr

  error = 0L

  u_pol  = pol[uniq(pol, sort(pol))]
  u_beam = beam[uniq(beam, sort(beam))]
  u_wave = beam[uniq(wave, sort(wave))]

  n_u_pol  = n_elements(u_pol)
  n_u_beam = n_elements(u_beam)
  n_u_wave = n_elements(u_wave)

  n_images = n_u_pol * n_u_beam * n_u_beam - 1L]

  new_images  = 0.0 * images[*, *, 0:n_images - 1L]
  new_headers = headers[*, 0:n_images - 1L]

  new_pol     = strarr(n_images)
  new_beam    = 0S * intarr(n_images)
  new_wave    = 0.0 * fltarr(n_images)

  i = 0L

  for p = 0L, n_u_pol - 1L do begin 
    for w = 0L, n_u_wave - 1L do begin
      for b = 0L, n_u_beam - 1L do begin
        ind = where(pol eq u_pol[p] and beam eq u_beam[b] and wave eq u_wave[w], $
                    count)

        ; if a pol-beam-wave combination is not found, exit with error
        if (count eq 0L) then begin
          error = 1L
          goto, done
        endif

        ; combine images by averaging images together
        new_images[i]  = mean(images[*, *, ind], dimension=3)

        ; combine headers, e.g., concatenating tags such as RAWEXT
        new_headers[i] = comp_combine_headers(headers, ind)

        new_pol[i]  = u_pol[p]
        new_beam[i] = u_beam[b]
        new_wave[i] = u_wave[w]

        i += 1
      endfor
    endfor
  endfor

  ; return new images and metadata
  images  = new_images
  headers = new_headers

  pol  = new_pol
  beam = new_beam
  wave = new_wave

  done:
end
