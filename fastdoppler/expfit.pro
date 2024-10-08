function expfit,r,parm
  ;parm is the form y=a*e^(-(x-1)/b)
  ;NOTE x is in SOLAR RADII
  output=parm(0)*exp(-(r-1)/parm(1))
  return,output
end
