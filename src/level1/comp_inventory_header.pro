; docformat = 'rst'

;+
; Procedure to inventory contents of comp file header array. Beam status, group
; association, wavelength, polarization state, data type and exposure time are
; returned, one for each header in the array.
;
; :Uses:
;   sxpar
;
; :Params:
;   headers : in, required, type="strarr(ntags, nimages)"
;     array of headers
;   beam : out, required, type=intarr(nimg)
;     the beam state (+1 has on-band image in lower right, off-band in upper
;     left, vice versa for -1).
;   wave : out, required, type=fltarr(nimg)
;     the wavelengths
;   pol : out, required, type=strarr(nimg)
;     polarization states (e.g., 'I+Q' or 'Q' and so on)
;   type : out, required, type=string
;     whether image is dark, flat ('OPAL') or sun data
;   expose : out, required, type=float
;     exposure time (ms)
;   cover : out, required, type=integer
;     cover status (0 -> dark, 1 -> flat)
;   cal_pol : out, optional, type=unknown
;     don't know what this one is...
;   cal_ret : out, optional, type=unknown  
;     unknown
;
; :Keywords:
;   group : out, optional, type=intarr(nimg)
;     group ID number; groups are defind to be unique combinations of
;     wavelength, beam, and polarization state.
;
; :Author:
;   MLSO Software Team
;-
pro comp_inventory_header, headers, beam, wave, pol, type, expose, $
                           cover, cal_pol, cal_ret, $
                           group=group
  compile_opt strictarr

  num = n_elements(headers[0L, *])   ; number of images in file

  beam  = intarr(num)
  wave  = fltarr(num)
  pol   = strarr(num)

  type = ''
  header = headers[*, 0L]

  ; type
  cover = sxpar(header, 'COVER')
  if (cover eq 0L) then begin
    type = 'DARK'
  endif else begin
    opal_value = sxpar(header, 'OPAL')
    if (opal_value eq 1L) then begin
      type = 'OPAL'
    endif else begin
      type = 'DATA'
    endelse
  endelse
  cal_pol = sxpar(header,'POLARIZR')

  cal_ret = sxpar(header, 'RETARDER', count=count)
  if (count eq 0L) then cal_ret = 0L

  ; other keywords
  for i = 0L, num - 1L do begin
    header = headers[*, i]
    beam[i] = sxpar(header, 'BEAM')
    wave[i] = sxpar(header, 'WAVELENG')
    pol[i] = strcompress(sxpar(header, 'POLSTATE'), /remove_all)
    expose = sxpar(header, 'EXPOSURE')
  endfor

  ; group observations with like wavelength, polarization state, datatype and
  ; beam

  if (arg_present(group)) then begin
    group = intarr(num)

    group[0L] = 0L
    num_groups = 1L

    for i = 1L, num - 1L do begin
      for j = 0L, i - 1L do begin
        if (wave[i] eq wave[j] and pol[i] eq pol[j] and beam[i] eq beam[j]) then begin
          group[i] = group[j]
          goto, done
        endif
      endfor
      group[i] = num_groups
      ++num_groups

      done:
    endfor
  endif
end
