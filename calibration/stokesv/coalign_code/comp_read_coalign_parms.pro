function comp_read_coalign_parms, parmfile, filenames

  	line=''
	nlines = file_lines(parmfile)
	filenames = strarr(nlines)
	parms = dblarr(nlines,4)

  	openr,lun,parmfile,/get_lun
  	for i=0,nlines-1 do begin
		readf,lun,line
		splitarr = strsplit(line,' ',/extract)
		filenames[i]=splitarr[0]
		for j=0,3 do parms[i,j] = double(splitarr[j+1])
	endfor
	free_lun,lun
	
	return,parms

end
	
