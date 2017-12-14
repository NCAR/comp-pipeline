; docformat = 'rst'

;+
; Create a mask of the annular field of view for CoMP images in the 1024x1024
; spatial resolution. Include the occulting disk and field stop only.
;
; :Uses:
;   sxpar
;
; :Params:
;   flat_header : in, required, type=strarr
;      header from flat image
;
; :Returns:
;   `fltarr(1024, 1024)`
;
; :Keywords:
;   o_offset : in, optional
;     radial offset of occulting disk (optional, 0. if not provided)
;   f_offset : in, optional
;     radial offset of field stop (optional, 0. if not provided)
;   uncorrected : in, optional, type=boolean
;     set to use the distortion uncorrected FITS keywords in `flat_header`
;
; :Author:
;   MLSO Software Team
;-
function comp_annulus_1024, flat_header, o_offset=o_offset, f_offset=f_offset, $
                            uncorrected=uncorrected
  compile_opt strictarr

  n = 1024
  mask_1024 = fltarr(n, n) + 1.

  x = rebin(findgen(n), n, n)
  y = transpose(x)

  ; get occulter positions and radii
  ox1 = sxpar(flat_header, keyword_set(uncorrected) ? 'OXCNTRU1' : 'OXCNTER1') - 1.0
  oy1 = sxpar(flat_header, keyword_set(uncorrected) ? 'OYCNTRU1' : 'OYCNTER1') - 1.0
  or1 = sxpar(flat_header, keyword_set(uncorrected) ? 'ORADU1' : 'ORADIUS1')

  ox2 = sxpar(flat_header, keyword_set(uncorrected) ? 'OXCNTRU2' : 'OXCNTER2') - 1.0
  oy2 = sxpar(flat_header, keyword_set(uncorrected) ? 'OYCNTRU2' : 'OYCNTER2') - 1.0
  or2 = sxpar(flat_header, keyword_set(uncorrected) ? 'ORADU2' : 'ORADIUS2')

  ; get field positions and radii
  fx1 = sxpar(flat_header, keyword_set(uncorrected) ? 'FXCNTRU1' : 'FXCNTER1') - 1.0
  fy1 = sxpar(flat_header, keyword_set(uncorrected) ? 'FYCNTRU1' : 'FYCNTER1') - 1.0
  fr1 = sxpar(flat_header, keyword_set(uncorrected) ? 'FRADU1' : 'FRADIUS1')

  fx2 = sxpar(flat_header, keyword_set(uncorrected) ? 'FXCNTRU2' : 'FXCNTER2') - 1.0
  fy2 = sxpar(flat_header, keyword_set(uncorrected) ? 'FYCNTRU2' : 'FYCNTER2') - 1.0
  fr2 = sxpar(flat_header, keyword_set(uncorrected) ? 'FRADU2' : 'FRADIUS2')

  if (n_elements(o_offset) gt 0) then drad_o = o_offset else drad_o = 0.
  if (n_elements(f_offset) gt 0) then drad_f = f_offset else drad_f = 0.

  ; mask outside of field mask
  r1 = sqrt((x - fx1)^2 + (y - fy1)^2)
  r2 = sqrt((x - fx2)^2 + (y - fy2)^2)
  good = where(r1 gt fr1 + drad_f and r2 gt fr2 + drad_f, count)
  if (count gt 0) then mask_1024[good] = 0.

  ; mask inside of occulter
  r1 = sqrt((x - ox1)^2 + (y - oy1)^2)
  r2 = sqrt((x - ox2)^2 + (y - oy2)^2)
  good = where(r1 lt or1 + drad_o or r2 lt or2 + drad_o, count)
  if (count gt 0) then mask_1024[good] = 0.

  return, mask_1024
end
