pro inventory,fcbin,beam,group,wave,pol,type,expose,cover,cal_pol,cal_ret,cal_ang=cal_ang

  COMPILE_OPT IDL2
  
  ;  procedure to inventory contents of comp data file,
  
  ;  beam status, group association, wavelength, polarization
  ;  state, data type and exposure time are returned
  
  ;  groups are defind to be unique combinations of wavelength, beam
  ;  and polarization state
  
  num=fcbin.nextend               ;number of images in file
  
  beam=intarr(num)
  group=intarr(num)
  wave=fltarr(num)
  pol=strarr(num)
  
  type = ''
  fits_read,fcbin,data,header,/header_only,exten_no=0
  
  ; Type
  cover = sxpar(header,'COVER')
  if cover eq 0 then begin
    type = 'DARK'
  endif else begin
    opal_value = sxpar(header,'OPAL')
    if opal_value eq 1 then begin
      type = 'OPAL'
    endif else begin
      type = 'DATA'
    endelse
  endelse
  cal_pol=sxpar(header,'POLARIZR')
  
  cal_ret=sxpar(header,'RETARDER', count=count)
  
  cal_ang = sxpar(header,'POLANGLE')
  if count eq 0 then cal_ret = 0
  
  ; Other keywords
  for i=0,num-1 do begin
    fits_read,fcbin,data,header,/header_only,exten_no=i+1
    beam[i]=sxpar(header,'BEAM')
    wave[i]=sxpar(header,'WAVELENG')
    pol[i]=strcompress(sxpar(header,'POLSTATE'),/remove_all)
    expose=sxpar(header,'EXPOSURE')
  endfor
  
  ;  group observations with like wavelength, polarization state, datatype and beam
  
  group[0]=0
  num_groups=1
  
  for i=1,num-1 do begin
    for j=0,i-1 do begin
      if wave[i] eq wave[j] and pol[i] eq pol[j] and beam[i] eq beam[j] then begin
        group[i]=group[j]
        goto,done
      endif
    endfor
    group[i]=num_groups
    num_groups=num_groups+1
    done:
  endfor
  
end