; docformat = 'rst'

;+
;
; :Uses:
;   comp_find_image_center
;
; :Params:
;   im : in, required, type="fltarr(620, 620)"
;     image to find the annulus within
;   occulter : out, optional, type=structure
;     structure of the form `{x:0., y:0., r:0.}`
;   field : out, optional, type=structure
;     structure of the form `{x:0., y:0., r:0.}`
;-
pro comp_find_annulus, im, occulter, field
  compile_opt idl2

  occulter_radius_guess = 226.
  field_radius_guess = 297.

  c_occulter = comp_find_image_center(im, radius_guess=occulter_radius_guess)
  ; set result if too far from guess
  if (c_occulter[2] gt 1.1 * occulter_radius_guess) then begin
    mg_log, 'c_occulter radius off', name='comp', /warn
    c_occulter[2] = occulter_radius_guess
  endif

  c_field = comp_find_image_center(im, radius_guess=field_radius_guess, /neg_pol)
  ; set result if too far from guess
  if (c_field[2] gt 1.1 * field_radius_guess) then begin
    mg_log, 'c_field radius off', name='comp', /warn
    c_field[2] = field_radius_guess
  endif

  s = size(c_occulter)
  if (s[0] eq 0) then begin
    mg_log, 'failed to find image center', name='comp', /warn
  endif

  occulter = {x:c_occulter[0], y:c_occulter[1], r:c_occulter[2]}
  field = {x:c_field[0], y:c_field[1], r:c_field[2]}
end
