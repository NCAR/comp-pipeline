pro taper, dat, length

;  procedure taper to taper the ends of time series with a cosine bell.
;  length is the number of points at each end for which the taper will
;  be applied.

no_zero=where( dat ne 0., count)
if count gt 0 then begin

  first=min(no_zero)
  if first+length le n_elements(dat) then begin
    taper=( 1.+cos( findgen(length)*!pi/(float(length-1))+!pi ) )/2.
    dat(first:first+length-1)=dat(first:first+length-1)*taper
  endif

  last=max(no_zero)
  if last-length+1 ge 0 then begin
    taper=( 1.+cos( findgen(length)*!pi/(float(length-1)) ) )/2.
    dat(last-length+1:last)=dat(last-length+1:last)*taper
  endif

endif

end
