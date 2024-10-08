function field_mask,nx,radius,dx=dx,dy=dy,top_left=top_left,bot_right=bot_right

;  procedure to create field mask
;  if dx and dy are present, the mask will be shifted by dx,dy

mask=fltarr(nx,nx)+1.

x=rebin(indgen(nx)-nx/2.,nx,nx)
y=transpose(x)
if n_elements(dx) gt 0 or n_elements(dy) gt 0 then begin
  x=x-dx
  y=y-dy
endif

r=sqrt(x^2+y^2)

if n_elements(top_left) gt 0 then tl=top_left else tl=407.
if n_elements(bot_right) gt 0 then br=bot_right else br=414.

bad=where(r lt radius or (y-x) gt tl or x-y gt br or abs(y+x) gt 540.,count)
if count gt 0 then mask(bad)=0.

return,mask
end