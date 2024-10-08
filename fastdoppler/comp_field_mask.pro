function comp_field_mask, radius,dx=dx,dy=dy
    
  ; procedure to create field mask
  ; if dx and dy are present, the mask will be shifted by dx,dy
  ; due to the field stop being off-center in the detector with respect to the occulter
    
  common comp_constants, nx, ny,center1074, center1079, center1083,stokes, n_stokes, debug, int_thresh, $
    diff_thresh
  
  mask=fltarr(nx,ny)+1.
  
  x=rebin(indgen(nx)-nx/2.,nx,ny)
  y=transpose(x)
  if n_elements(dx) gt 0 or n_elements(dy) gt 0 then begin
    x=x-dx
    y=y-dy
  endif
  
  r=sqrt(x^2+y^2)
  
  bad=where(r gt radius,count) 
  if count gt 0 then mask[bad]=0.
  
  return,mask
end
