;+
; Name: estimate_quantile
;
; Estimates the quantiles of the data array x_in using a cumulative histogram. Executes in O(N)
; operations, so perfomance is acceptable even for very large arrays (e.g., large images).
; Binning is done in log space, so relatively few bins are required even if the range of x_in
; is very large.
;
; Inputs:
;	
;	x_in: The input array, which may have arbitrary dimensions.
;	quantile: The quantile desired
;
; Optional Inputs:
;	
;	nbins:	The number of bins to use. More is better, and data with a wider range will
; 		  	require more bins to achieve the same accuracy. Default 1000, minimum 5.
;	hmin:	Maximum value to use for histogram. Default is maximum in array.
;	hmin:	Minimum value to use for histogram. Default is minimum in array.
;
; Joseph Plowman (plowman@physics.montana.edu) 09-14-12
;-
function estimate_quantile, x_in, quantile, nbins=nbins, hmin=hmin, hmax=hmax

	xmax = max(x_in,/nan)
	xmin = min(x_in,/nan)
	if(n_elements(nbins) eq 0) then nbins = 1.0e3
	if(nbins lt 5) then nbins = 5
	hoffset = 0.0

	; Edge checking for arrays with all the same number or all infinity/nan entries:
	if(xmax eq xmin or (finite(xmax) eq 0 and finite(xmin) eq 0)) then begin
		quantile_estimate = xmin
		return,quantile_estimate
	endif
	
	
	if(xmax ne xmin) then begin
		if(n_elements(hmin) eq 0) then hmin = xmin
		if(n_elements(hmax) eq 0) then hmax = xmax
		
		; Switch to log space, offsetting the horizontal scale so the array is all positive:
		hoffset = xmin-0.1*abs(xmin)-10
		loghmin = alog10(hmin-hoffset)
		loghmax = alog10(hmax-hoffset)
		x = alog10(x_in-hoffset)
		
		; Setup the histogram range:
		xrange = loghmax-loghmin
		binsize = (loghmax-loghmin)/nbins
		nbins = (loghmax-loghmin)/binsize
		if(nbins gt nbins) then binsize = (loghmax-loghmin)/nbins
		
		; Compute the histogram:
		hist = histogram(x,min=loghmin,max=loghmax,binsize=binsize,locations=bins)
		nbin = n_elements(hist)
		
		; Transform to cumulative histogram:
		chist = total(hist,/cumulative)
		
		; Make cumulative histogram start at 0 and end at 1:
		chist = [0,chist]
		chist /= max(chist)
		
		; Make x axis of cumulative histogram consistent with above:
		bins = [bins,bins(nbin-1)+binsize]

		; Interpolate to find the x axis value corresponding to the desired quantile level:
		quantile_estimate=interpol(bins,chist,quantile)

	endif

	; Transform back from log space:
	return,10^(quantile_estimate)+hoffset
	
end
