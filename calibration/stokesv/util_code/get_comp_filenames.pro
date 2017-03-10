function get_comp_filenames, directory, goodfiles, wavestr, fits_suffix, taitimes=taitimes, dates=dates, times=times

	if(n_elements(fits_suffix) eq 0) then fits_suffix = '.comp.'+wavestr+'.fts.gz'

	openr,lun,directory+goodfiles,/get_lun
	line=''
	while ~eof(lun) do begin &$
		readf,lun,line &$
		line = file_basename(line) &$
		if(n_elements(lines) gt 0) then lines=[lines,line] &$
		if(n_elements(lines) eq 0) then lines=line &$
	endwhile
	free_lun,lun

	nfiles = n_elements(lines) ; Find how many files there are...

	dates=strarr(nfiles)
	times=strarr(nfiles)
	types=strarr(nfiles)
	ntunes = lonarr(nfiles)
	ccsdstimes=strarr(nfiles)
	filenames = strarr(nfiles)

	; Read the lines to get the times. Could maybe use SXPAR for this, but I didn't know about it at the time.
	for i=0,nfiles-1 do begin &$
		splitarr = strsplit(lines[i],' ',/extract)
		datetimes = strsplit(splitarr[0],'.',/extract)
		dates[i] = datetimes[0]
		times[i] = datetimes[1]
		ccsdstimes[i] = strmid(dates[i],0,4)+'-'+strmid(dates[i],4,2)+'-'+strmid(dates[i],6,2)+'T'
		ccsdstimes[i] += strmid(times[i],0,2)+':'+strmid(times[i],2,2)+':'+strmid(times[i],4,2)+'.500Z'
	endfor

	; Create Dates
	taitimes = anytim2tai(ccsdstimes)
	timesort = sort(taitimes)
	taitimes = taitimes[timesort]
	dates = dates[timesort]
	times = times[timesort]

	; Combine the dates, times, etc to create the corresponding filepaths:
	filepaths = directory+dates+'.'+times+fits_suffix

	return, filepaths
	
end