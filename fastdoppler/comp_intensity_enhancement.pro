function comp_intensity_enhancement, data, hdr

  if (sxpar(hdr,'FRADIUS') ne 0 and sxpar(hdr,'ORADIUS') ne 0) then begin
    r_inner = sxpar(hdr,'ORADIUS')
    r_outer = sxpar(hdr,'FRADIUS')
  endif else begin
    r_inner  = (sxpar(hdr,'OCRAD1')+sxpar(hdr,'OCRAD2'))/2.
    r_outer  = ((sxpar(hdr,'FCRAD1')+sxpar(hdr,'FCRAD2'))/2.)+10.
  endelse
  xycenter = [sxpar(hdr,'CRPIX1'),sxpar(hdr,'CRPIX2')]
  
  ss = size(data)
  v_x = double(findgen(ss[1]))
  v_y = double(findgen(ss[2]))
  x = rebin(v_x,ss[1],ss[2])-xycenter[0]
  y = rebin(transpose(v_y),ss[1],ss[2])-xycenter[1]
  r=sqrt(x^2+y^2)
  
  sr=sort(r)
  sx=r(sr)
  
  sim=data(sr)
  lminp=min(where(sx ge r_inner))
  maxp =max(where(sx le r_outer))
  lx1=sx(lminp:maxp)
  ly1=sim(lminp:maxp)
  err=1D
  start=double([1E+6,50])
  
  lfit=mpfitfun('expfit',lx1,ly1,err,start,/quiet)
  limb=bytscl(data/expfit(r,lfit),min=0,max=4)
  mlimb=unsharp_mask(limb,amount=5)
  
  fitim=mlimb*(r ge r_inner-1)
  
  return, fitim
end
