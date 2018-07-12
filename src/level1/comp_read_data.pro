; docformat = 'rst'

;+
; Reads a CoMP data file (although it's really pretty generic; main quirk is
; that the data headers start at extension 1 and index zero is primary header
; only).
;
; :Uses:
;   fits_open, fits_read, fits_close
;
; :Params:
;   filename : in, required, type=string
;     the name of the CoMP FITS file to be read
;   images : out, required, type=numeric array nx*ny*nextend
;     the array of images read from the file, starting with extension 1
;   headers : out, required, type=numeric array ntags*nextend
;     the fits headers for each image in the array
;   header0 : out, optional, type=numeric array
;     the primary header (extension 0) from the file; generally, the same
;     information will also be included in each element of the extension header
;     array
;
; :Author:
;   MLSO Software Team
;-
pro comp_read_data, filename, images, headers, header0
  compile_opt strictarr
  on_error, 2

  fits_open, filename, fcb
  nxt = fcb.nextend

  mg_log, '%d images in file', nxt, name='comp', /debug

  if (nxt le 1) then message, filename + ' contains no data'

  ; get the primary header
  fits_read, filename, image0, header0, exten_no=0, /header_only, /no_abort, message=msg
  if (msg ne '') then message, msg

  ; loop through the extensions
  for i = 1L, nxt do begin
    fits_read, fcb, image, header, exten_no=i, /pdu, /no_abort, message=msg
    if (msg ne '') then message, msg
    if (i eq 1L) then begin   ; do setup if this is the first extension...
      images = dblarr(n_elements(image[*, 0]), n_elements(image[0, *]), nxt)
      headers = strarr(n_elements(header), nxt)
    endif

    ; Assign the current extension and header to their part in the array.
    ; Note that if the current extension has a different number of elements
    ; than the initial one, it won't be assigned (although the header will
    ; still be filled). 
    if (n_elements(images[*, *, i - 1]) eq n_elements(image)) then begin
      images[*, *, i - 1] = image
    endif

    for j = 0L, n_elements(header) - 1L do headers[j, i - 1] = header[j]
  endfor

  fits_close, fcb

  ; repair issues in raw data
  mg_log, 'repair raw data with %s', raw_data_repair_routine, name='comp', /debug
  call_procedure, raw_data_repair_routine, images, headers, header0
end
