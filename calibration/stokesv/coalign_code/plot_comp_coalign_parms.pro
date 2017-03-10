pro plot_comp_coalign_parms, parmfile, times=times, use_filenames=use_filenames

	parms = load_coalign_parameters(parmfile,files=datafiles)
	
	nfiles = n_elements(datafiles)
	
	times = dblarr(nfiles)
	for i=0,nfiles-1 do begin
		if(keyword_set(use_filenames)) then begin
			sa = strsplit(file_basename(datafiles[i]),'.',/extract)
			date = strmid(sa[0],0,4)+'-'+strmid(sa[0],4,2)+'-'+strmid(sa[0],6,2)
			time = strmid(sa[1],0,2)+':'+strmid(sa[1],2,2)+':'+strmid(sa[1],4,2)
		endif else begin
			comp_read_data,datafiles[i],images,headers,header0
			date = sxpar(header0,'DATE-OBS')
			time = sxpar(header0,'TIME-OBS')
		endelse
		times[i] = anytim2tai(date+'T'+time)
		print,times[i]
	endfor
	
	times -= times[0]
	!p.multi = [0,2,2]
	plot,times,parms[*,0],title=['Horizontal shift'],xtitle='Seconds',ytitle='Arc-seconds'	
	plot,times,parms[*,1],title=['Vertical shift'],xtitle='Seconds',ytitle='Arc-seconds'
	plot,times,parms[*,2],title=['Rotation'],xtitle='Seconds',ytitle='Degrees'
	plot,times,parms[*,3],title=['Goodness of fit'],xtitle='Seconds',ytitle='Chi Squared'
	
end