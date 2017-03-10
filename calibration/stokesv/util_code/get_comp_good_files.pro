function get_comp_good_files, gbufile, dates=dates, times=times, sigmas=sigmas, nwaves=nwaves, ps=ps, bgs=bgs

  	line=''
	nlines = file_lines(gbufile)
	gbu = strarr(nlines)
  	lines = strarr(nlines)

  	openr,lun,gbufile,/get_lun
  	for i=0,nlines-1 do begin
		readf,lun,line
		lines[i] = line
		splitarr = strsplit(line,' ',/extract)
		gbu[i]=splitarr[1]
	endfor
	lines=lines[1:nlines-1]
	wgood = where(gbu eq 'Good',ngood)
	lines = lines[wgood]
	ngood=n_elements(lines)

	filenames = strarr(ngood)
	dates = strarr(ngood)
	times = strarr(ngood)
	ps = strarr(ngood)
	bgs = strarr(ngood)
	sigmas = fltarr(ngood)
	nwaves = lonarr(ngood)
	for i=0,ngood-1 do begin
		splitarr = strsplit(lines[i],' ',/extract)
		filenames[i]=splitarr[0]
		bgs[i] = splitarr[2]
;		sigmas[i] = splitarr[3]
		nwaves[i] = splitarr[4]

		fnsplit = strsplit(filenames[i],'.',/extract)
		dates[i] = fnsplit[0]
		times[i] = fnsplit[1]
		ps[i] = fnsplit[4]
	endfor
	free_lun,lun
	
	return,filenames

end
	
