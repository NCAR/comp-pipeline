; docformat = 'rst'
;+
; get_comp_component
;
;	Retrieves a specified CoMP component (i.e., polarization
;	state and FG/BG beam setting) from a set of images and
;	headers.
;
;	:Returns:
;		Array of images at the specified polarization states,
;		beam settings, and (optionally) wavelengths.
;
;	:Params:
;		images: in, required, type = real array nx*ny*nimg
;			The array of images from which to retrieve
;			the desired component.
;		headers: in, required, type = string array ntag*nimg
;			The header array corresponding to images
;		polstate: in, required, type = string
;			The desired polarization state (e.g., 'I+V' or 'Q')
;		beam: in, required, type = integer
;			The desired beam setting (+1 or -1). If this
;			data has beams combined (level 1 or greater),
;			set beam to 0.
;		wave: in, optional, type = real scalar or array
;			The desired wavelength(s). If undefined, all
;			wavelengths will be returned. If wave is passed
;			in but undefined, it will contain the wavelengths
;			on return.
;
;	:Keywords:
;		skipall: in, optional, type = flag
;			Skip the first image at every wavelength.
;		count: out, optional, type = long
;			The number of images averaged at each wavelength
;		headersout: out, optional, type = string array varies*nimg
;			An updated set of headers (adds or updates the 'NAVERAGE'
;			flag).
;		wavavg: in, optional, type = flag
;			Average over wavelengths.
;		noskip: in, optional, type = flag
;			Don't skip the very first image (due to an instrument
;			issue, the very first image in each raw file is bad).
;
;	:Author:
;		Joseph Plowman
;-
function get_comp_component, images, headers, polstate, beam, wave, skipall=skipall, count=count, $
		headersout=headersout, wavavg=wavavg, noskip=noskip

	; Figure out what's in this image array:
	inventory_header,headers,beams,groups,waves,polstates,type,expose,cover,cal_pol,cal_ret
	
	; If we don't have an input list of wavelengths, use all of them:
	if(n_elements(wave) eq 0) then wave = waves[uniq(waves,sort(waves))]
	nw = n_elements(wave)	
	ntags = n_elements(headers[*,0])
	; If this header is missing NAVERAGE (e.g., it's a raw file), the output
	; header will need another tag:
	if(sxpar(headers[*,0],'NAVERAGE') eq 0) then ntags++
	
	; Flags to check if polarization states and beams match:
	check1 = polstates eq polstate and beams eq beam
	; Skip very first image, which is bad due to instrument issue:
	if(keyword_set(skipall) eq 0 and keyword_set(noskip) eq 0) then check1[0] = 0
	
	count = lonarr(nw)
	imgout = images[*,*,0:nw-1]
	headersout = strarr(ntags,nw)
	
	; Loop over unique wavelengths:
	for i=0,nw-1 do begin
		; Find which indices have matching wavelength, polstate, and beam:
		checki = where(check1 and waves eq wave[i],counti)
		imagei = images[*,*,checki]
		if(counti lt 1) then message,'get_comp_component error: No image at specified polarization/beam/wave!'
		if(keyword_set(skipall)) then begin
			imagei = imagei[*,*,1:counti-1] ; Skip first image at all wavelengths...
			counti--
		endif
		; Average over images with same wavelengths, polstate, and beam:
		if(counti gt 1) then imagei = mean(imagei,dimension=3)
		imgout[*,*,i] = imagei

		; Update headers:
		if(sxpar(headers[*,0],'NAVERAGE') eq 0) then begin
			count[i] = counti
		endif else begin
			for j=0,n_elements(checki)-1 do count[i]+=sxpar(headers[*,checki[j]],'NAVERAGE')
		endelse
		headertemp = headers[*,checki[0]]
		sxaddpar,headertemp,'NAVERAGE',count[i], " Number of images averaged together", AFTER="EXPOSURE"
		headersout[*,i] = headertemp
	endfor
	
	; Average over all wavelengths if wavavg is set:
	if(keyword_set(wavavg) and nw gt 1) then begin
		imgout = mean(imgout,dimension=3)
		count = total(count)
		headersout = headersout[*,0]
		sxaddpar,headersout,'NAVERAGE',count, " Number of images averaged together", AFTER="EXPOSURE"
	endif
	
	return, imgout
	
end
