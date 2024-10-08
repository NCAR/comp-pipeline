pro plot_st_radial

dir='/DAta/wave/process/20140113/stplot/0.03_1'
;dir='/Volumes/E/wave/process/20140107/stplot/0.007'
cd,dir

window,0,xs=1600,ys=800, retain=2
posi=[0.05,0.05,0.16,0.95]
openr,1,'stplot.txt'
str=''
for i=0,7 do begin
  readf,1,str
  restore,str+'/stplot_cut=0.sav'
  int1=int_slice[*,*]
  int1_sort=int1(sort(int1)) & np=n_elements(int1[where(finite(int1))])
  dminmax = [int1_sort[np*0.01], int1_sort[np*0.99]]
  plot_image,transpose(int1)<dminmax[1]>dminmax[0],/noerase,xtickformat='(a1)',xticklen=0.0001,ytickformat='(a1)',yticklen=0.0001,$
       charsize=1.0,position=posi+[i*0.11,0,i*0.11,0]
ENDFOR
write_png,dir+'stplot_1.png', tvrd(/true), r, g, b
close,1
end
