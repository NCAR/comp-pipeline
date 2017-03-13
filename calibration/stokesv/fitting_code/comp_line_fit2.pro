pro comp_line_fit, waves, intens, vars, parms, parmerr, chisq, fit

	sinc_sdev_approx = wfilt/sqrt(2.0*!pi)
	
	wfilt = 2.3D/16.0D
	
	nt = n_elements(waves)
	nstokes = floor(n_elements(intens)/nt)

	; When computing the initial guess, want to avoid the wing channels, which have issues where the signal is weak:
	wavindex_lo = 1
	wavindex_hi = nt-2

	waves = reform(waves)
	intens = reform(intens)
	vars = reform(vars)

	intenI = intens[0:nt-1]
	slopeI = 1.0*(intenI[wavindex_hi]-intenI[wavindex_lo])/(waves[wavindex_hi]-waves[wavindex_lo])
;	bgI = 0.25*(intenI[nt-1]+intenI[0]); 
	bgI = 0.5*min(intenI) > 0
	
	if(nstokes eq 4) then begin
		intenV = intens[3*nt:4*nt-1]
		vbgguess = 0.75*(intenV[wavindex_hi]+intenV[wavindex_lo])/(intenI[wavindex_hi]+intenI[wavindex_lo]); /intenI[nt-1]+intenV[0]/intenI[0])
	endif
;	vbgguess = 0.5*(intenV[nt-1]/intenI[nt-1]+intenV[0]/intenI[0])
;	vbgguess = 0.5*(0.5*(intenV[nt-1]+intenV[0])/bgI + (intenV[nt-1]-intenV[0])/(intenI[nt-1]-intenI[0]))

	intenI -= slopeI*(waves-waves[2])
	intenI -= bgI
	
	norm = int_tabulated(waves,intenI)
	mean = int_tabulated(waves,waves*intenI)/norm
	sdev = sqrt(int_tabulated(waves,waves*waves*intenI)/norm-mean*mean)
	efunc = exp(-0.5*(waves-mean)*(waves-mean)/(sdev*sdev))
	
	sdev = sqrt(sdev^2.0-sinc_sdev_approx^2.0)

	facs = comp_inten_calc(waves-mean,sdev*(1.0+dblarr(nt)))
	facs2 = comp_inten_calc(waves-mean,sdev*(1.0+dblarr(nt)),/deriv)

	if(nstokes eq 3) then amps = dblarr(nstokes)
	if(nstokes eq 4) then amps = dblarr(nstokes+1)
	for i=0,nstokes-1 do begin
		inten = reform(intens[i*nt:i*nt+nt-1])
		if(i eq 0) then inten -= bgI+slopeI*(waves-waves[2])
		if(i eq 3) then inten -= vbgguess*(bgI+slopeI*(waves-waves[2]))
		var = vars[i*nt:i*nt+nt-1]
		ampnum = inten*facs/var
		ampden = facs^2.0/var
		amps[i] = total(ampnum[wavindex_lo:wavindex_hi])/total(ampden[wavindex_lo:wavindex_hi])
;		amps[i] = total(inten*facs/var)/total(facs^2.0/var)
		if(i eq 3) then begin
;			amps[i] = 0.0
			ampnum = inten*facs2/var
			ampden = facs2^2.0/var
			amps[i] = total(ampnum[wavindex_lo:wavindex_hi])/total(ampden[wavindex_lo:wavindex_hi])
;			amps[i+1] = total(inten*facs2/var)/total(facs2^2.0/var)
		endif
	endfor
	
;	slopeI = 0
	vbgguess=0
	if(nstokes eq 4) then parms = [mean,sdev,bgI/wfilt,slopeI/wfilt,amps,vbgguess]
	if(nstokes eq 3) then parms = [mean,sdev,bgI/wfilt,slopeI/wfilt,amps]
	
	parmerr = 0.0*parms
	comp_stokes_profiles, waves, parms, fit	
	chisq = total((fit-intens)^2.0/vars)
	fita = [1,1,1,1,1,1,1,1,1,1]
	
	fit = curvefit(waves, intens, 1.0/vars, parms, parmerr, function_name='comp_stokes_profiles_derivs', /double, chisq=chisq,fita=fita, status=status)
	fit = [fit,status]

end
