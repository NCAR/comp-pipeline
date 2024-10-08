function comp_overlap_mask, radius, angle, dx = dx, dy = dy
    
  ; procedure to overlap of field mask
  ; if dx and dy are present, the mask will be shifted by dx,dy
  ; due to the field stop being off-center in the detector with respect to the occulter
    
  common comp_constants
  common mask_constants

  mask=fltarr(nx,ny)+1.
  
  x=rebin(indgen(nx)-nx/2.,nx,ny)
  y=transpose(x)
  if n_elements(dx) gt 0 or n_elements(dy) gt 0 then begin
    x=x-dx
    y=y-dy
  endif
  
  r=sqrt(x^2+y^2)
  
  ; mask out field stop overlap
  ; lower right
  x0 = (2*radius - field_overlap) * cos((360.-angle)/!RADEG)
  y0 = (2*radius - field_overlap) * sin((360.-angle)/!RADEG)
  mask[ where( sqrt( (x-x0)^2+(y-y0)^2 ) lt radius,count) ]=0.
  
  ; upper left
  x0 = (2*radius - field_overlap) * cos((180.-angle)/!RADEG)
  y0 = (2*radius - field_overlap) * sin((180.-angle)/!RADEG)
  mask[ where( sqrt( (x-x0)^2+(y-y0)^2 ) lt radius,count) ]=0.
  
  return,mask

end
