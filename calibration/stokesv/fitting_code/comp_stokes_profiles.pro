pro comp_stokes_profiles, waves, params, stokes

	nstokes = ((n_elements(params)-4) < 4) > 1
	
	wfilt = 2.3D/16.0D
		
	ntune = n_elements(waves)
	wcen = waves(floor(0.5*ntune))
	
	mean = params[0]
	sdev = params[1]
	Ioff = params[2]
	if(n_elements(params) gt 4) then begin
		Islp = params[3]
		amps = params[4:4+nstokes-1]
	endif else begin
		Islp = 0.0D
		amps = params[3]
	endelse
	if(nstokes eq 4) then begin
		vmag = params[4+nstokes]
		linearbg_ItoV = params[5+nstokes]
	endif

	linepro = comp_inten_calc(waves-mean,sdev+dblarr(ntune))
	
	stokes = dblarr(ntune*nstokes)
	
	for i=0,nstokes-1 do begin
		stokes[i*ntune:(i+1)*ntune-1] = amps[i]*linepro
	endfor
	
	linearbg_I = (Islp*(waves-wcen)+Ioff)*wfilt
	linearbg_V = (linearbg_ItoV*(waves-wcen))*wfilt
	stokes[0:ntune-1] += linearbg_I
	if(nstokes eq 4) then begin
		vlinepro = comp_inten_calc(waves-mean,sdev+dblarr(ntune),/deriv)
		stokes[(nstokes-1)*ntune:(nstokes)*ntune-1] += vmag*vlinepro
;		stokes[(nstokes-1)*ntune:(nstokes)*ntune-1] += linearbg_ItoV
		stokes[(nstokes-1)*ntune:(nstokes)*ntune-1] += linearbg_ItoV*linearbg_I
;		stokes[(nstokes-1)*ntune:(nstokes)*ntune-1] += linearbg_ItoV*stokes[0:ntune-1]
;		stokes[(nstokes-1)*ntune:(nstokes)*ntune-1] = vmag*vlinepro
;		stokes[(nstokes-1)*ntune:(nstokes)*ntune-1] += linearbg_ItoV*linepro
;		stokes[(nstokes-1)*ntune:(nstokes)*ntune-1] += amps[3]/amps[0]*stokes[0:ntune-1]
	endif
			
end

pro comp_stokes_profiles_derivs, waves, params, stokes, derivs

	comp_stokes_profiles, waves, params, stokes
	
	nterms = n_elements(params)
	npoint = n_elements(stokes)
	
	derivs = dblarr(npoint,nterms)

	diff = (3.0D)*(10.0D)^(-6.0D)
	
	for i=0,nterms-1 do begin
		paramsplus = params
		paramsminus = params			
		if(params[i] eq 0) then begin
			paramsplus[i] += diff
			paramsminus[i] -= diff
		endif else begin
			paramsplus[i] += diff*params[i]
			paramsminus[i] -= diff*params[i]
		endelse
		comp_stokes_profiles, waves, paramsplus, stokesplus
		comp_stokes_profiles, waves, paramsminus, stokesminus
		derivs[*,i] = (stokesplus-stokesminus)/(paramsplus[i]-paramsminus[i])
	endfor
	
end
		
