; docformat = 'rst'

;+
; Find the annulus in a flat.
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
;
; :Keywords:
;   occulter_guess : in, optional, type=fltarr(3)
;     guess for center/radius of occulter, in the order x, y, r
;   field_guess : in, optional, type=fltarr(3)
;     guess for center/radius of field, in the order x, y, r
;   occulter_only : in, optional, type=boolean
;     set to only find occulter center and radius, skipping field
;   error : out, optional, type=long
;     0 if no error
;
; :Author:
;   MLSO Software Team
;-
pro comp_find_annulus, im, occulter, field, $
                       occulter_guess=occulter_guess, $
                       field_guess=field_guess, $
                       occulter_only=occulter_only, $
                       error=error, $
                       occulter_points=occulter_points, field_points=field_points
  compile_opt idl2

  error = 0L
  if (n_elements(occulter_guess) eq 0L) then begin
    occulter_radius_guess = 226.0
  endif else begin
    occulter_center_guess = occulter_guess[0:1]
    occulter_radius_guess = occulter_guess[2]
  endelse

  c_occulter = comp_find_image_center(im, $
                                      center_guess=occulter_center_guess, $
                                      radius_guess=occulter_radius_guess, $
                                      error=error, points=occulter_points)
  if (error ne 0L) then begin
    mg_log, 'error finding occulter center', name='comp', /warn
    error = 1L
    return
  endif

  ; set result if too far from guess
  if (c_occulter[2] gt 1.1 * occulter_radius_guess) then begin
    mg_log, 'c_occulter radius off', name='comp', /warn
    c_occulter[2] = occulter_radius_guess
  endif

  if (~keyword_set(occulter_only)) then begin
    if (n_elements(field_guess) eq 0L) then begin
      field_radius_guess = 297.0
    endif else begin
      field_center_guess = field_guess[0:1]
      field_radius_guess = field_guess[2]
    endelse

    c_field = comp_find_image_center(im, $
                                     center_guess=field_center_guess, $
                                     radius_guess=field_radius_guess, $
                                     /neg_pol, $
                                     error=error, points=field_points)
    if (error ne 0L) then begin
      mg_log, 'error finding field center', name='comp', /warn
      error = 1L
      return
    endif

    ; set result if too far from guess
    if (c_field[2] gt 1.1 * field_radius_guess) then begin
      mg_log, 'c_field radius off', name='comp', /warn
      c_field[2] = field_radius_guess
    endif

    field = {x:c_field[0], y:c_field[1], r:c_field[2]}
  endif

  s = size(c_occulter)
  if (s[0] eq 0) then begin
    mg_log, 'failed to find image center', name='comp', /warn
  endif

  occulter = {x:c_occulter[0], y:c_occulter[1], r:c_occulter[2]}
end
