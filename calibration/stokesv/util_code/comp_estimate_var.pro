function comp_estimate_var_old, imagearr, headerarr, nt, weights=weights, noisefloor=noisefloor, relerrmin=relerrmin

	nxt = n_elements(headerarr[0,*])
	navg = lonarr(nxt)
	for i=0,nxt-1 do navg[i] = sxpar(headerarr[*,i],'NAVERAGE')

	vararr = imagearr
	nstokes = round(n_elements(navg)/nt)-1
;	photfac = double(1.0/sqrt(875.0))
	photfac = double(1.0/sqrt(8750.0))
	bgs = imagearr[*,*,nstokes*nt:nt*(nstokes+1)-1]
	nbgframes = navg[nstokes*nt:nt*(nstokes+1)-1]
	noisefloor = 1.0

	if(n_elements(weights) eq 0) then weights = 1.0+dblarr(nstokes)
	for i=0,nstokes-1 do begin
		for j=0,nt-1 do begin
			stokes = abs(imagearr[*,*,i*nt+j])
			nstokesframes = navg[i*nt+j]
			fgvar = abs(imagearr[*,*,j])*photfac/nstokesframes/2.0
;			fgvar = 2.0*abs(imagearr[*,*,j])*nstokesframes/photfac*(0.5*photfac/nstokesframes)^2.0
			bgvar = abs(bgs[*,*,j])*photfac/nstokesframes/2.0
;			bgvar = abs(bgs[*,*,j])*nbgframes[j]/photfac*(photfac/nbgframes[j])^2.0
			vararr[*,*,i*nt+j] = weights[i]*(fgvar + bgvar + noisefloor*photfac^2.0/nstokesframes)
		endfor
	endfor

	if(n_elements(relerrmin) eq 0) then relerrmin = 0.00001
	smallvars = where(sqrt(vararr)/abs(imagearr) lt relerrmin,count)
	if(count gt 0) then vararr[smallvars] = (relerrmin*abs(imagearr[smallvars]))^2.0

	return, vararr
	
end

function comp_estimate_var, imagearr, headerarr, nt, weights=weights, noisefloor=noisefloor, relerrmin=relerrmin, polstrs=polstrs

	nimg = n_elements(imagearr[0,0,*])
;	photfac = double(1.0/sqrt(8750.0))
	photfac = double(1.0/875.0)
	vararr = imagearr
	if(n_elements(noisefloor) eq 0) then noisefloor = 1.0
;	if(n_elements(weights) eq 0) then weights = [1.0,20.0,20.0,25.0]
	if(n_elements(weights) eq 0) then weights = [1.0,1.0,1.0,1.0]
	if(n_elements(polstrs) eq 0) then polstrs = ['I','Q','U','V']
	inventory_header,headerarr,beams,groups,waves,polstates,type,expose,cover,cal_pol,cal_ret
	uwaves = waves[uniq(round(waves*100),sort(waves))]
	nuwave = n_elements(uwaves)
	bgs = dblarr(nuwave)
	for i=0,nimg-1 do begin
		bg = sxpar(headerarr[*,i],'BACKGRND')
		iwav = where(uwaves eq waves[i])
		if(bg gt bgs(iwav[0])) then bgs[iwav[0]]=bg
	endfor
	print,'comp_estimate_var background levels: ',bgs,uwaves

	for i=0,nimg-1 do begin
		pol = strmid(polstates[i],strlen(polstates[i])-1)
		polindex = where(polstrs eq pol)
		fg = get_comp_component(imagearr,headerarr,'I',0,waves[i],/noskip) + noisefloor*photfac
;		fg = imagearr[*,*,i] + noisefloor*photfac
		fg += noisefloor*photfac
		nframes = sxpar(headerarr[*,i],'NAVERAGE')
		if(strlen(polstates[i]) eq 1) then begin
;			bg = get_comp_component(imagearr,headerarr,'B'+pol,0,waves[i],/noskip,headersout=bgheader)
;			bg = get_comp_component(imagearr,headerarr,'BI',0,waves[i],/noskip)
;			bg += noisefloor*photfac
			iwav = where(uwaves eq waves[i])
			bg = bgs[iwav[0]]
;			nbgframes = sxpar(bgheader,'NAVERAGE')
		endif else begin
			bg = 0*fg
			nbgframes = 1.0
		endelse
		fgvar = abs(fg)*photfac/nframes
		bgvar = abs(bg)*photfac/nframes
		vararr[*,*,i] = weights[polindex[0]]*(fgvar + bgvar)
	endfor
	
	if(n_elements(relerrmin) eq 0) then relerrmin = 0.00001
	smallvars = where(sqrt(vararr)/abs(imagearr) lt relerrmin,count)
	if(count gt 0) then vararr[smallvars] = (relerrmin*abs(imagearr[smallvars]))^2.0

	return, vararr

end	

function comp_estimate_var2, fgimagearr, fgheaderarr, bgimagearr, bgheaderarr, weights=weights, noisefloor=noisefloor, relerrmin=relerrmin, polstrs=polstrs

	nimg = n_elements(fgimagearr[0,0,*])
	photfac = double(1.0/875.0)
	vararr = fgimagearr
	if(n_elements(noisefloor) eq 0) then noisefloor = 1.0
	if(n_elements(weights) eq 0) then weights = [1.0,1.0,1.0,1.0]
	if(n_elements(polstrs) eq 0) then polstrs = ['I','Q','U','V']
	inventory_header,fgheaderarr,beams,groups,waves,polstates,type,expose,cover,cal_pol,cal_ret

	for i=0,nimg-1 do begin
		pol = strmid(polstates[i],strlen(polstates[i])-1)
		polindex = where(polstrs eq pol)
		fg = get_comp_component(fgimagearr,fgheaderarr,'I',0,waves[i],/noskip) + noisefloor*photfac
		fg += noisefloor*photfac
		nframes = sxpar(fgheaderarr[*,i],'NAVERAGE')
		bg = get_comp_component(bgimagearr,bgheaderarr,'BKGI',0,waves[i],/noskip)
		fgvar = abs(fg)*photfac/nframes
		bgvar = abs(bg)*photfac/nframes
		vararr[*,*,i] = weights[polindex[0]]*(fgvar + bgvar)
	endfor
	
	if(n_elements(relerrmin) eq 0) then relerrmin = 0.00001
	smallvars = where(sqrt(vararr)/abs(fgimagearr) lt relerrmin,count)
	if(count gt 0) then vararr[smallvars] = (relerrmin*abs(fgimagearr[smallvars]))^2.0

	return, vararr

end	
