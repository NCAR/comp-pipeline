function comp_inten_calc, lambda0, sigma, wfilt=wfilt, recalc=recalc, deriv=deriv

	common comp_inten_calc_comblk, interptab, interptab_deriv, na, nl0, nl, logamin, logamax, l0min, l0max, lmin, lmax

        save_file = 'inten_calc_comblk.sav'
        if (n_elements(interptab) eq 0L) then begin
          if (file_test(save_file)) then begin
            mg_log, 'restoring %s', save_file
            restore, save_file
            interptab = struct.interptab
            interptab_deriv = struct.interptab_deriv
            na = struct.na
            nl0 = struct.nl0
            nl = struct.nl
            logamin = struct.logamin
            logamax = struct.logamax
            l0min = struct.l0min
            l0max = struct.l0max
            lmin = struct.lmin
            lmax = struct.lmax
          endif else mg_log, '%s does not exist', save_file
        endif

	if(n_elements(wfilt) eq 0) then wfilt = 2.3D
	pi = acos(0.0D)*2.0D
	
	nmfac = 1.0e-7
	
	alpha = sigma/(sigma+wfilt/16.0D)
	
	lambda0p = lambda0/(sigma+wfilt/16.0D)
	
	if(n_elements(interptab) eq 0 or keyword_set(recalc)) then begin
		na = 1000
		nl0 = 1000
		nl = 1000L
		l0min = -3.0D*pi
		l0max = 3.0D*pi
		logamin = alog(0.001D)
		logamax = alog(0.999D)
		lmin = -6.0D*pi
		lmax = 6.0D*pi
		alphas = exp(logamin+(logamax-logamin)*findgen(na)/(na-1.0D))
		lambda0s = l0min+(l0max-l0min)*findgen(nl0)/(nl0-1.0D)
		lambdas = lmin+(lmax-lmin)*findgen(nl)/(nl-1.0D)
		interptab = dblarr(nl0,na)
		interptab_deriv = interptab
		print,'Calculating interpolation table for CoMP response:'
		for i=0,nl0-1 do begin
			for j=0,na-1 do begin
				lambdascal = min([alphas[j],(1.0D)-alphas[j]])
				lambdac = lambdas*lambdascal+lambda0s[i]*((1.0D)-alphas[j])
				integrand = sinc(lambdac/((1.0D)-alphas(j)))^2.0D*exp(-0.5D*((lambdac-lambda0s[i])/alphas[j])^2.0D)
				integrand_deriv = -integrand*(lambdac-lambda0s[i])/(alphas[j])^2.0D
				interptab[i,j] = int_tabulated(lambdac, integrand,/double)/sqrt(2.0D*pi)/alphas[j]
				interptab_deriv[i,j] = int_tabulated(lambdac, integrand_deriv,/double)/sqrt(2.0D*pi)/alphas[j]
			endfor
		endfor
		print,'Done calculating interpolation table.'
	endif
	
	ialpha = (alog(alpha)-logamin)*(na-1.0D)/(logamax-logamin)
	ilambda0 = (lambda0p-l0min)*(nl0-1.0D)/(l0max-l0min)
	
	inten = 0.0D*alpha
	
	if(keyword_set(deriv)) then begin
;		integral = interpolate(interptab_deriv,ilambda0,ialpha,cubic=-0.5D)*(sigma+wfilt/16.0D)/sigma/sigma/nmfac
		integral = interpolate(interptab_deriv,ilambda0,ialpha,cubic=-0.5D)/(sigma+wfilt/16.0D)/nmfac
	endif else begin
		integral = interpolate(interptab,ilambda0,ialpha,cubic=-0.5D)
	endelse

        if (~file_test(save_file)) then begin
          mg_log, 'saving interptab in %s', save_file
          struct = {interptab:interptab, $
                    interptab_deriv:interptab_deriv, $
                    na:na, $
                    nl0:nl0, $
                    nl:nl, $
                    logamin:logamin, $
                    logamax:logamax, $
                    l0min:l0min, $
                    l0max:l0max, $
                    lmin:lmin, $
                    lmax:lmax}
          save, struct, filename=save_file
        endif

	
	return,integral
	
end
		
function comp_inten_calc_slow, lambdas_in, sigmas_in, wfilt=wfilt, deriv=deriv

	if(n_elements(wfilt) eq 0) then wfilt = 2.3


	nl0 = 1000L
	nl = 2*nl0
	l0min = -3.0*!pi
	l0max = 3.0*!pi

	nlam = n_elements(lambdas_in)
	intens = dblarr(nlam)
	
	for i=0,nlam-1 do begin
		lambda0 = lambdas_in[i]
		sigma = sigmas_in[i]
		alpha = sigma/(sigma+wfilt/16.0)
		lmin = 2.0*l0min;*(sigma+wfilt/16.0)
		lmax = 2.0*l0max;*(sigma+wfilt/16.0)
		lambdas = (lmin+(lmax-lmin)*findgen(nl)/(nl-1.0))*min([sigma,wfilt/16.0])+lambda0*(1.0-alpha)	
		integrand = sinc(lambdas/wfilt*16.0)^2*exp(-0.5*((lambdas-lambda0)/sigma)^2)
		if(keyword_set(deriv)) then integrand *= -(lambdas-lambda0)/sigma^2
		intens[i] = int_tabulated(lambdas, integrand)/sqrt(2*!pi)/sigma
	endfor
	

	return, intens
	
end
