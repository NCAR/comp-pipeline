; docformat = 'rst'

;+
; Compute the rest wavelength.
;
; Returns NaN if no valid east and west pixels.
;
; :Params:
;   primary_header : in, required, type=strarr
;     primary FITS header, needed to produce geometric mask
;   velocity : in, required, type="fltarr(nx, ny)"
;     center lines velocity to compute rest wavelength from
;   intensity : in, required, type="fltarr(nx, ny, 3)"
;     center lines intensity for thresholding
;   line_width : in, required, type="fltarr(nx, ny)"
;     center lines line width for thresholding
;
; :Returns:
;   indices : out, optional, type=lonarr
;     indices into velocity used in calculation, and valid to apply rest
;     wavelength to
;-
function comp_compute_rest_wavelength, primary_header, $
                                       velocity, $
                                       intensity, $
                                       line_width_fwhm, $
                                       method=method, $
                                       indices=indices, $
                                       med_east=med_east, med_west=med_west
  compile_opt strictarr

  _method = n_elements(method) eq 0L ? 'median' : method

 ;calling comp_l2_mask with more restrictive  requirements than nornal level2  images
 mask = comp_l2_mask(primary_header, /img_occulter_radius, occulter_offset=4, field_offset=-10)

  dims = size(velocity, /dimensions)
  nx = dims[0]
  ny = dims[1]
  x = findgen(nx) - (nx - 1.0) / 2.0
  x = rebin(reform(x, nx, 1), nx, ny)

;GdT revised velocity criteria 
; it assumes: 
; velocity is in km/s and zeros in velocity are invalid pixels
; line width is the FWHM in km/s 
; note the condition in velocity is NOT simmetric because the line is blue-shifted

;set lower maximum for 1079 - to use later
  lambda_zero=sxpar(primary_header, 'WAVE_REF')
  rest_int_max = 2.0 if lambda_zero gt 1076 then rest_int_max = 1.0
      
  threshold_condition = mask gt 0 $
                          and velocity ne 0 $
                          and velocity gt -30 $
                          and velocity lt 20 $
                          and intensity[*, *, 0] gt 0.5 $
                          and intensity[*, *, 1] gt rest_int_max $
                          and intensity[*, *, 2] gt 0.5 $
                          and intensity[*, *, 0] lt 60.0 $
                          and intensity[*, *, 1] lt 60.0 $
                          and intensity[*, *, 2] lt 60.0 $
                          and line_width_fwhm gt 35.0 $
                          and line_width_fwhm lt 120.0
  
  indices = where(threshold_condition, /null)

  east = where(threshold_condition and x lt 0.0, n_east, /null)
  west = where(threshold_condition and x gt 0.0, n_west, /null)

; TODO
; n_east or n_west equal to zero should never happen unless the image is bad
; if this happens the function should not return the west(east) velocity 
; the velocity and line width should not be computed at all for that image
; 
; TODO
; if not enough pixels in the east(west) are found the function should not return 
; the west(east) velocity 
; we should have a file with the rest wavelength from the median file of the day and
; use that value if n_west or n_east are less than ~1000 pixels 
; NOTE: this should happen rarely
; to make this less likely the minimum intensity for 1079 can be set to1 instead of 2


  if (n_east eq 0L or n_west eq 0L) then return, !values.f_nan

  if (_method eq 'mean') then begin

    rest_velocity = mean([mean([velocity[east]], /nan), $
                            mean([velocity[west]], /nan)] )
  endif else begin
     med_east  =  median( [velocity[east]] )
     med_west =  median( [velocity[west]] )
     
    rest_velocity = 0.5*(med_east  + med_west )
  endelse

  return, rest_velocity
end
