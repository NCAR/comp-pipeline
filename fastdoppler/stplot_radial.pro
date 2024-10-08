; NAME:
;
; stplot_curve
;
; PURPOSE:
;
; Interpolate the data along several selected curves, then plot space-time maps for these curved-cuts
;
; CATEGORY:
;
; Data analysis
;
; CALLING SEQUENCES:
;
; stplot_curve,data
;
; stplot_curve,data,dir_sav=dir_sav,xtitle=xtitle,ytitle=ytitle,log=log,sm_scl=sm_scl,npt=npt,plotfrm=plotfrm,panel_len=panel_len
;
; INPUTS:
;
; data:       an image sequence [nx,ny,nt]
;
; KEYWORDS:
;
; dir_sav:    the directory where the resulting images and data will be saved, e.g., './sampledata/SOT_CaII_20070114/'
;
; xtitle:     lable of the horizontal axis (time) of the space-time plot, e.g., 'Time (s)'
;
; ytitle:     lable of the vertical axis (distance) of the space-time plot, e.g., 'Distance (arcsec)'
;
; log:        take common logarithm of the intensity if set
;
; sm_scl:     the smooth scale used to generate the trend which will be removed from the original time series
;
; npt:        the number of clicked points for each curve line, >=4
;
; plotfrm:    the index of the image shown, default is 0 (the first image in the sequence)
;
; panel_len:  the size of one panel in the horizontal dimension (time)
;
; dpercent:   the dynamic range of the image to be shown, in unit of percentage, e.g., [0.01,0.99]
;
; tr:         range of time frames to be plotted, default is [0,nt-1]
;
; OUTPUTS:
;
; the following images and data files will be created:
;
; stplot_cuts.png:         the locations of the selected cuts ploted on the first image in the image sequence
;
; stplot_cutslocation.sav: the locations of the selected cuts (xc, yc) and number of cuts (ncut)
;
; stplot_cut=i.sav: the space-time data array for cut i
;
; stplot.png:       the original and de-trended space-time plots for all cuts
;
; HISTORY:
;
; Written by Hui Tian at CfA, 2 May 2013


pro stplot_radial,time_str,dir_sav=dir_sav,xtitle=xtitle,ytitle=ytitle,log=log,sm_scl=sm_scl,npt=npt,$
  plotfrm=plotfrm,panel_len=panel_len,dpercent=dpercent,tr=tr

restore,'/Data/wave/process/20140113/'+time_str+'_vel_filtered.sav'

data=velocity

if not keyword_set(dir_sav) then dir_sav='/Data/wave/process/20140113/stplot/0.03_1/'+time_str+'/'

if not keyword_set(xtitle) then xtitle='Time step'

if not keyword_set(ytitle) then ytitle='Spatial pixel'
ytitle=['',ytitle]

if not keyword_set(sm_scl) then sm_scl=5

if not keyword_set(npt) then npt=5

if not keyword_set(plotfrm) then plotfrm=0

if not keyword_set(panel_len) then panel_len=400

if not keyword_set(dpercent) then dpercent=[0.01,0.99]

nt=(size(data))[3]

if not keyword_set(tr) then tr=[0,nt-1]

;select a curved line by clicking npt positions, more than one curved lines can be selected
ncut=1  ;the number of cuts


xi=fltarr(ncut,npt)  &  yi=fltarr(ncut,npt)
window,0,xs=700,ys=700, retain = 2  &  !p.multi=0
bad=where(msk eq 0, complement=good)

  if keyword_set(log) then img=alog10(reform(data[*,*,plotfrm])) else img=reform(data[*,*,plotfrm])
  img_sort=(img[good])(sort(img[good])) & np=n_elements((img[good])[where(finite(img[good]))])
  dminmax = [img_sort[np*dpercent[0]], img_sort[np*dpercent[1]]]
  plot_image,img<dminmax[1]>dminmax[0],title='select a curved line by clicking npt positions, more than one curved lines can be selected'
  for j=0,ncut-1 do begin
  for i=0,npt-1 do begin
  ; theta=-57.+(80./npt)*i
  ; xx=67.5+243*(1.-cos(theta/180.*!pi))    ;1.1 R_sun, 20140113_E
  ; yy=310.5+243*sin(theta/180.*!pi)

  ; theta=-45.+(80./npt)*i
  ; xx=67.5+243*(1.-cos(theta/180.*!pi))    ;1.1 R_sun, 20140110_E
  ; yy=310.5+243*sin(theta/180.*!pi)

  theta=-50.+(80./npt)*i
  xx=553.5-243*(1.-cos(theta/180.*!pi))    ;1.1 R_sun, 20140113_W
  yy=310.5+243*sin(theta/180.*!pi)

  ; theta=-40.+(60./npt)*i
  ; xx=553.5-243*(1.-cos(theta/180.*!pi))    ;1.1 R_sun, 20140110_W
  ; yy=310.5+243*sin(theta/180.*!pi)

  plots,xx,yy,/data,psym=5,symsize=0.6
  print,'x/y:',xx,yy
  xi(j,i)=xx  &  yi(j,i)=yy
  endfor
  oplot,xi(j,*),yi(j,*),thick=2,linestyle=2,color=255 ;plot these 10 points
  ;xyouts,xi(j,0),yi(j,0),'cut '+strtrim(string(j),2),alignment=1.0
  endfor

write_png,dir_sav+'stplot_cuts.png', tvrd(/true), r, g, b
save,filename=dir_sav+'stplot_cutslocation.sav',xi,yi,ncut

window,1,xs=panel_len*ncut,ys=600,xpos=0,ypos=0, retain = 2  &  !p.multi=0
posi=[0.07,0.07,0.97,0.95]
for j=0,ncut-1 do begin
  x=interpol(xi[j,*],10*npt,/spline)
  y=interpol(yi[j,*],10*npt,/spline)
  dis1=dblarr(10*npt-1)
  for i=1,10*npt-1 do dis1[i-1]=sqrt((x[i]-x[i-1])^2.+(y[i]-y[i-1])^2.)
  dis=total(dis1)  ;length of the selected curve line, in the unit of spatial pixel
  xtrack=interpol(x,dis,/spline)
  ytrack=interpol(y,dis,/spline)
  int_slice=fltarr(dis,nt)
  for i=0,nt-1 do begin
    img=reform(data(*,*,i))
    ;int_slice(*,i)=interpolate(img,xtrack,ytrack,cubic=-0.5)
    int_slice(*,i)=(interpolate(img,xtrack,ytrack,cubic=-0.5)+interpolate(img,xtrack-1,ytrack-1,cubic=-0.5)$
      +interpolate(img,xtrack+1,ytrack+1,cubic=-0.5)+interpolate(img,xtrack-1,ytrack+1,cubic=-0.5)$
      +interpolate(img,xtrack+1,ytrack-1,cubic=-0.5))/5.
  endfor

 if keyword_set(log) then int_slice=alog10(int_slice)

 ;original intensity
 int1=int_slice[*,tr[0]:tr[1]]
 int1_sort=int1(sort(int1)) & np=n_elements(int1[where(finite(int1))])
 dminmax = [int1_sort[np*dpercent[0]], int1_sort[np*dpercent[1]]]
 plot_image,transpose(int1)<dminmax[1]>dminmax[0],xtitle=xtitle,ytitle=ytitle[j eq 0],/noerase,xticklen=-0.020,xminor=10,$
      title='cut '+strtrim(string(j),2),charsize=1.0,position=posi

 ; ;detrended intensity
 ; int2=(int1-smooth(int1,[1,sm_scl],/EDGE_TRUNCATE,/NAN))/smooth(int1,[1,sm_scl],/EDGE_TRUNCATE,/NAN) ;detrend and normalize
 ; int2_sort=int2(sort(int2)) & np=n_elements(int2[where(finite(int2))])
 ; dminmax = [int2_sort[np*dpercent[0]], int2_sort[np*dpercent[1]]]
 ; plot_image,transpose(int2)<dminmax[1]>dminmax[0],xtitle=xtitle,ytitle=ytitle[j eq 0],/noerase,xticklen=-0.020,xminor=10,$
 ;      title='cut '+strtrim(string(j),2)+' - detrended',charsize=1.0,position=posi-[0,0.5,0,0.5]+[0.9/ncut,0,0.9/ncut,0]*j

 save,filename=dir_sav+'stplot_cut='+strtrim(string(j),2)+'.sav',int_slice
endfor

write_png,dir_sav+'stplot.png', tvrd(/true), r, g, b


end
