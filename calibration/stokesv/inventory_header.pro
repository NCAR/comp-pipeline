;+
; inventory_header
;
;	Procedure to inventory contents of comp file header array.
;	Beam status, group association, wavelength, polarization
;	state, data type and exposure time are returned, one for
;	each header in the array.
;
;	Inputs:
;		headers: in, required, type = string array (ntags*nimg)
;			Array of headers.
;
;	Outputs:
;		beam: out, required, type = integer array (nimg)
;			The beam state (+1 has on-band image in lower right,
;			off-band in upper left, vice versa for -1).
;		group: out, required, type = integer array (nimg)
;			Group ID number; groups are defind to be unique combinations
;			of wavelength, beam, and polarization state.
;		wave: out, required, type = float array (nimg)
;			The wavelengths.
;		pol: out, required, type = string array (nimg)
;			Polarization states (e.g., 'I+Q' or 'Q' and so on).
;		type: out, required, type = string
;			Whether image is dark, flat ('OPAL') or sun data.
;		expose: out, required, type = number
;			Exposure time (ms)
;		cover: out, required, type = integer
;			Cover status (0 -> dark, 1 -> flat)
;		cal_pol: out, optional, type = unknown
;			Don't know what this one is...
;		cal_ret: out, optional, type = unknown	
;			''
;
;	:Author:
;		Steve Tomczyk, Joseph Plowman
;-
pro inventory_header,headers,beam,group,wave,pol,type,expose,cover,cal_pol,cal_ret

  COMPILE_OPT IDL2
  
  num=n_elements(headers[0,*])               ;number of images in file
  
  beam=intarr(num)
  group=intarr(num)
  wave=fltarr(num)
  pol=strarr(num)
  
  type = ''
  header = headers[*,0]
  
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
  if count eq 0 then cal_ret = 0
  
  ; Other keywords
  for i=0,num-1 do begin
    header = headers[*,i]
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