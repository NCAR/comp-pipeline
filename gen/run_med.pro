function run_med, array, width
  compile_opt strictarr
;  run_min procedure to compute running median of array, using
;  window width wide

n=n_elements(array)
rmed=fltarr(n)
w2=fix(width/2)

for i=0,n-1 do begin
  i1=i-w2 > 0
  i2=i+w2 < n-1
  rmed[i]=median(array[i1:i2])
endfor

return,rmed
end
