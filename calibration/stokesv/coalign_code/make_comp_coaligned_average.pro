function make_comp_coaligned_average, filenames, ref_file, coalign_parms, headerout=headerout, $
		header0=header0, chi2_thold=chi2_thold, pols=pols, waves=waves, varout=varout
		
	if(n_elements(pols) eq 0) then pols = ['I','Q','U','V']
	if(n_elements(waves) eq 0) then waves = ['1074.38', '1074.50', '1074.62', '1074.74', '1074.86']
	if(n_elements(chi2_thold) eq 0) then chi2_thold = 5.0
	
	npol = n_elements(pols)
	nwav = n_elements(waves)
	nframes = npol*nwav
	naverages = lonarr(nframes)
	nimg = lonarr(nframes)

	comp_read_data,filenames[ref_file],images_ref, headers_ref, header0_ref
	ntref = max([sxpar(header0_ref,'NTUNE'),sxpar(header0_ref,'NTUNES')])
	refindex = convert_ascii_header(headers_ref[*,floor(0.5*ntref)])
	nx = n_elements(images_ref[*,0,0])
	ny = n_elements(images_ref[0,*,0])
	imageout = dblarr(nx,ny,nframes)
	varout = dblarr(nx,ny,nframes)
	ntags = n_elements(headers_ref[*,0])
	if(sxpar(headers_ref[*,0],'NAVERAGE') eq 0) then ntags++
	headerout = strarr(ntags,nframes)
	
	chi2s = coalign_parms[*,3]
	goodchi2s = where(chi2s lt chi2_thold)
	ngood = n_elements(goodchi2s)
	
	for i=0,ngood-1 do begin
		ii = goodchi2s[i]
		print,'Adding data from '+filenames[ii]+' to coaligned average'
		comp_read_data, filenames[ii], images, headers, header0
		inventory_header,headers,beams,groups,file_waves,file_pols,type,expose,cover,cal_pol,cal_ret
		print,file_waves
		print,file_pols
		for j=0,npol-1 do begin
			for k=0,nwav-1 do begin
				jk = j*nwav+k
				jj = where(file_waves eq waves[k] and file_pols eq pols[j], count)
				if(count gt 0) then begin
					index = convert_ascii_header(headers[*,jj])
					navg = sxpar(headers[*,jj],'NAVERAGE')
					naverages[jk] += sxpar(headers[*,jj],'NAVERAGE')
					image = comp_coreg_resamp(images[*,*,jj], index, refindex, $
							coalign_parms[ii,*])
					imageout[*,*,jk] += navg*image
					; Accumulate the variance array. It might seem like this should be weighted by navg, but that
					; underestimates the variance, since navg*image*image has less variation than the sum squared
					; of the original images (which are navg in number). The variance estimated in this way will be 
					; too low by a factor of navg. So an extra factor of navg is applied here and the formula which 
					; computes the variance from these contains an extra factor of <navg> (see below)...
					varout[*,*,jk] += navg*navg*image*image					
					print,max(images[*,*,jj]),max(imageout[*,*,jk]),navg
					header = headers[*,jj]
					sxaddpar,header,'NAVERAGE',naverages[jk]
					headerout[*,jk] = header
					nimg[jk] = nimg[jk]+1
				endif
			endfor
		endfor
	endfor
		
	print,naverages
	for j=0,nframes-1 do begin
		imageout[*,*,j] /= 1.0*naverages[j]
		varout[*,*,j] = (varout[*,*,j]/naverages[j]-(naverages[j]/nimg[j])*imageout[*,*,j]^2)/(1.0*naverages[j])
	endfor
	return,imageout
		
end

