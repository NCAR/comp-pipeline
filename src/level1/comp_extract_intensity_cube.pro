; docformat = 'rst'

;+
; Extract the cube of intensity images from a FITS file.
;
; :Uses:
;   comp_constants_common, comp_config_common, fits_open, fits_read, fits_close,
;   sxpar
;
; :Params:
;   filename : in, required, type=string
;     filename for a level 1 FITS file
;
; :Keywords:
;   images : out, optional, type="fltarr(620, 620, n_waves)"
;     intensity images
;   wavelengths : out, optional, type=fltarr(n_waves)
;     wavelengths of returned images
;   primary_header : out, optional, type=strarr
;     primary header
;   headers : out, optional, type="strarr(n_keywords, n_waves)"
;     extension headers
;
; :Author:
;   sitongia
;-
pro comp_extract_intensity_cube, filename, $
                                 images=images, $
                                 wavelengths=wavelengths, $
                                 primary_header=primary_header, $
                                 headers=headers
  compile_opt idl2
  @comp_constants_common
  @comp_config_common

  fits_open, filename, fcbin  ; open input fits file
  
  ; assume I images are always at beginning of file and read until non-I
  fits_read, fcbin, data, primary_header, exten_no=0

  n_extensions = fcbin.nextend   ; number of images in file

  ; assume all images are I, just to limit sizes of arrays
  wavelengths = fltarr(n_extensions)
  images = fltarr(nx, ny, n_extensions)

  ; extract I images
  for i = 1L, n_extensions do begin
    fits_read, fcbin, data, header, exten_no=i
    if (i eq 1) then headers = strarr(n_elements(header), n_extensions)

    wavelength = sxpar(header, 'WAVELENG')
    polstate   = strtrim(sxpar(header, 'POLSTATE'), 2)

    if (polstate eq 'I') then begin
      ; save the wavelength and data
      wavelengths[i - 1L] = wavelength
      images[*, *, i - 1L] = data
      headers[*, i - 1L] = header
    endif else break
  endfor

  ; trim the array to just I images
  wavelengths = wavelengths[0:i - 2]
  images = images[*, *, 0:i - 2]
  headers = headers[*, 0:i - 2]

  fits_close, fcbin
end
