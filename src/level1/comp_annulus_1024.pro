; docformat = 'rst'

;+
; Create a mask of the annular field of view for CoMP images in the 1024x1024
; spatial resolution. Include the occulting disk and field stop only.
;
; :Params:
;   flat_header : in, required, type=strarr
;      header from flat image
;
; :Keywords:
;   o_offset : in, optional
;     radial offset of occulting disk (optional, 0. if not provided)
;   f_offset : in, optional
;     radial offset of field stop (optional, 0. if not provided)
;
; :Author:
;   Tomczyk
;-
function comp_annulus_1024, flat_header, o_offset=o_offset, f_offset=f_offset 
  compile_opt strictarr

  n = 1024
  mask_1024 = fltarr(n, n) + 1.

  x = rebin(findgen(n), n, n)
  y = transpose(x)

  ; get occulter positions and radii
  ox1 = sxpar(flat_header, 'OXCNTER1')
  oy1 = sxpar(flat_header, 'OYCNTER1')
  or1 = sxpar(flat_header, 'ORADIUS1')

  ox2 = sxpar(flat_header, 'OXCNTER2')
  oy2 = sxpar(flat_header, 'OYCNTER2')
  or2 = sxpar(flat_header, 'ORADIUS2')

  ; get field positions and radii
  fx1 = sxpar(flat_header, 'FXCNTER1')
  fy1 = sxpar(flat_header, 'FYCNTER1')
  fr1 = sxpar(flat_header, 'FRADIUS1')

  fx2 = sxpar(flat_header, 'FXCNTER2')
  fy2 = sxpar(flat_header, 'FYCNTER2')
  fr2 = sxpar(flat_header, 'FRADIUS2')

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
