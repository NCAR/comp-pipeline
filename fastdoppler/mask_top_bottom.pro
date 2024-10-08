function mask_top_bottom,nx,ny,nm

;  function to create mask with zeroes at nm rows at the top and bottom of the mask

msk=fltarr(nx,ny)

y=transpose(rebin(findgen(ny),ny,nx))

one=where(y gt nm and y lt (ny-nm))

msk[one]=1.

return,msk
end