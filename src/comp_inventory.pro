; docformat = 'rst'

;+
; Procedure to inventory contents of comp data file, beam status, group
; association, wavelength, polarization state, data type and exposure time are
; returned.
;
; Groups are defind to be unique combinations of wavelength, beam and
; polarization state.
;
; :Uses:
;   fits_read, sxpar
;
; :Params:
;   fcbin : in, required, type=structure
;     FITS Control Block as returned by `FITS_OPEN`
;   beam : out, required, type=intarr
;     set to a named variable to retrieve the beam state (-1 or 1) for each
;     extension
;   wave : out, required, type=fltarr
;     set to a named variable to retrieve the wavelength (nm) for each extension
;   pol : out, required, type=strarr
;     set to a named variable to retrieve the polarization state of each
;     extension
;   type : out, required, type=string
;     set to a named variable to retrieve the type of the file, i.e., 'DARK',
;     'OPAL', or 'DATA'
;   expose : out, required, type=float
;     set to a named variable to retrieve the exposure in milliseconds
;   cover : out, required, type=integer
;     set to a named variable to retrieve whether the cover is present
;   cal_pol : out, required, type=integer
;     set to a named variable to retrieve whether the polarizer is present
;   cal_ret : out, required, type=integer
;     set to a named variable to retrieve whether the retarder is present
;
; :Keywords:
;   group : out, optional, type=intarr
;     group of each extension
;
; :Author:
;   MLSO Software Team
;-
pro comp_inventory, fcbin, beam, wave, pol, type, expose, cover, $
                    cal_pol, cal_ret, $
                    group=group, error=error
  compile_opt idl2

  error = 0L

  num = fcbin.nextend               ; number of images in file

  beam = intarr(num)
  wave = fltarr(num)
  pol = strarr(num)

  type = ''
  fits_read, fcbin, data, header, /header_only, exten_no=0, $
             message=message, /no_abort
  if (message ne '') then begin
    mg_log, 'error reading FITS file: %s', message, name='comp', /error
    error = 1L
    return
  endif

  ; type
  cover = sxpar(header, 'COVER')
  if (cover eq 0) then begin
    type = 'DARK'
  endif else begin
    opal_value = sxpar(header, 'OPAL')
    if (opal_value eq 1) then begin
      type = 'OPAL'
    endif else begin
      type = 'DATA'
    endelse
  endelse
  cal_pol = sxpar(header, 'POLARIZR')

  cal_ret = sxpar(header, 'RETARDER', count=count)
  if (count eq 0) then cal_ret = 0

  ; other keywords
  for i = 0L, num - 1L do begin
    fits_read, fcbin, data, header, /header_only, exten_no=i + 1
    beam[i] = sxpar(header, 'BEAM')
    wave[i] = sxpar(header, 'WAVELENG')
    pol[i] = strcompress(sxpar(header, 'POLSTATE'), /remove_all)
    expose = sxpar(header, 'EXPOSURE')
  endfor

  if (arg_present(group)) then begin
    ; group observations with like wavelength, polarization state, datatype and
    ; beam
    group = intarr(num)

    group[0] = 0
    num_groups = 1

    for i = 1L, num - 1L do begin
      for j = 0L, i - 1L do begin
        if (wave[i] eq wave[j] and pol[i] eq pol[j] and beam[i] eq beam[j]) then begin
          group[i] = group[j]
          goto, done
        endif
      endfor
      group[i] = num_groups
      num_groups = num_groups + 1
      done:
    endfor
  endif
end
