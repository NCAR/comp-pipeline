; docformat = 'rst'

;+
; Procedure to inventory contents of comp L1 data file.
;
; :Uses:
;   fits_read, sxpar
;
; :Params:
;   fcbin : in, required, type=structure
;     FITS identifier from `FITS_OPEN`
;   wave : out, optional, type=fltarr
;     set to a named variable to retrieve the wavelengths available in a CoMP
;     L1 data file
;   pol : out, optional, type=strarr
;     set to a named variable to retrieve the polarization states available in
;     CoMP L1 data file
;
; :Author:
;   MLSO Software Team
;-
pro comp_inventory_l1, fcbin, wave, pol
  compile_opt idl2

  fits_read, fcbin, data, header, /header_only, exten_no=0

  wave = fltarr(fcbin.nextend)
  pol  = strarr(fcbin.nextend)

  for i = 1L, fcbin.nextend do begin
    fits_read, fcbin, data, header, /header_only, exten_no=i
    wave[i - 1] = sxpar(header, 'WAVELENG')
    pol[i - 1]  = strcompress(sxpar(header, 'POLSTATE'), /remove_all)
  endfor
end
