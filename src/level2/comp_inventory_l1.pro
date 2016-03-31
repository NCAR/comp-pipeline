; docformat = 'rst'

;+
; Procedure to inventory contents of comp L1 data file.
;-
pro comp_inventory_l1, fcbin, wave, pol
  compile_opt idl2

  fits_read, fcbin, data, header, /header_only, exten_no=0

  num_wave = sxpar(header, 'NTUNE')

  num=fcbin.nextend - num_wave   ; number of images in file

  wave = fltarr(num)
  pol = strarr(num)

  ; TODO including background introduces zeros into these arrays
  for i = 0L, num - 1L do begin
    fits_read, fcbin, data, header, /header_only, exten_no=i + 1
    wave[i] =sxpar(header, 'WAVELENG')
    pol[i] = strcompress(sxpar(header, 'POLSTATE'), /remove_all)
  endfor
end
