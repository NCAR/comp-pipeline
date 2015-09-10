; docformat = 'rst'

;+
; Extract the cube of intensity images from a FITS file.
;
; :Uses:
;   comp_constants_common, comp_paths_common, fits_open, fits_read, fits_close,
;   sxpar
;
; :Params:
;   filename
;   images
;   waves
;
; :Author:
;   sitongia
;-
pro comp_extract_intensity_cube, filename, images, waves
  compile_opt idl2
  @comp_constants_common
  @comp_config_common

  fits_open, filename, fcbin  ; open input fits file
  
  ; sssume I images are always at beginning of file and read until non-I
  fits_read, fcbin, data, primary_header, exten_no=0

  numExtensions = fcbin.nextend   ; number of images in file

  ; assume all images are I, just to limit sizes of arrays
  waves = fltarr(numExtensions)
  images = fltarr(nx, ny, numExtensions)

  ; extract I images
  for n = 1L, numExtensions do begin
    fits_read, fcbin, data, header, exten_no=n

    wavelength = sxpar(header, 'WAVELENG')
    polstate   = strtrim(sxpar(header, 'POLSTATE'), 2)

    if (polstate eq 'I') then begin
      ; save the wavelength and data
      waves[n - 1L] = wavelength
      images[*, *, n - 1L] = data
    endif
  endfor

  ; trim the array to where the I images are
  noWaves = where(waves eq 0.0)
  nWaves = noWaves[0]
  waves = waves[0L:nWaves - 1L]

  fits_close, fcbin
end
