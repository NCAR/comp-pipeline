pro plot_filtered_velmap,time_str
dir='/Data/wave/process/20140113/'
; dir='/Volumes/E/wave/process/20140107/'
out_dir='/Data/wave/process/20140113/filtered_0.03/'
;out_dir='/Volumes/E/wave/process/20140107/'+time_str+'/filtered_0.03/'
cd,dir
restore,time_str+'_vel_filtered.sav'

num=(size(velocity))[3]
bad=where(msk eq 0, complement=good)

for i=0,num-1 do begin
  wset,0
  v=velocity[*,*,i]*1e9
  ;v=v-mean(v)
  v[bad]=-100
  s=(v[good])(sort(v[good]))
  img1=bytscl(v,min=-1.6,max=2)
  tvscl, img1

  out_name = out_dir + string(format='(i4.4,".bmp")',i)
	write_bmp,out_name,img1
ENDFOR

print,'done'

end
