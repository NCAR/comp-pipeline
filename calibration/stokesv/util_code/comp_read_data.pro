pro comp_read_data, filename, images, headers, header0

	fits_open,filename,fcb
	nxt = fcb.nextend
	
	if(nxt le 1) then message,string('Error in comp_read_data: ',filename,' contains no data!')
	
	fits_read,filename,image0,header0,exten_no=0,/header_only
	
	for i=1,nxt do begin
		fits_read,fcb,image,header,exten_no=i,/pdu
		if(i eq 1) then begin
			images=dblarr(n_elements(image[*,0]),n_elements(image[0,*]),nxt)
			headers=strarr(n_elements(header),nxt)
		endif
		if(n_elements(images[*,*,i-1]) eq n_elements(image)) then images[*,*,i-1] = image
		for j=0,n_elements(header)-1 do headers[j,i-1] = header[j]
	endfor
	
	fits_close,fcb
	
end
			