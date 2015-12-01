; docformat = 'rst'

;+
; CoMP has the local time in the header and the keywords use non-standard
; underscores. Convert local time to UT and add header keywords with dashes.
;
; :Uses:
;   sxpar, sxaddpar, sxdelpar, tim2carr
;
; :Params:
;   header : in, out, type=strarr
;     FITS header
;
; :Author:
;   sitongia
;
; :History:
;   changed DATE_OBS to DATE_HST   Oct 2 2014    GdT
;   changed TIME_OBS to TIME_HST   Oct 2 2014    GdT
;-
pro comp_fix_header_time, header
  compile_opt strictarr

  ; get CoMP values (in HST)
  hst_date_str = sxpar(header, 'DATE_OBS') ; '10/6/2010'
  hst_time_str = sxpar(header, 'TIME_OBS') ; '7:34:38 AM'

  result = strsplit(hst_date_str, '/', /extract)
  year   = result[2]
  month  = result[0]
  day    = result[1]

  result = strsplit(hst_time_str, ' ', /extract)
  time   = result[0]
  amORpm = result[1]

  result = strsplit(time, ':', /extract)
  hour   = result[0]
  minute = result[1]
  second = result[2]

  jd = julday(month, day, year, hour, minute, second)

  ; add 12 hours if PM
  if (amORpm eq 'PM' and hour lt 12) then begin
    jd += 0.5
  endif

  ; add 10 hours get UT
  jd += 10. / 24.

  caldat, jd, utmonth, utday, utyear, uthour, utminute, utsecond

  utyear =   string(utyear, format='(I4)')
  utday =    string(utday, format='(I02)')
  utmonth =  string(utmonth, format='(I02)')
  uthour =   string(uthour, format='(I02)')
  utminute = string(utminute, format='(I02)')
  utsecond = string(round(utsecond), format='(I02)')

  ; construct FITS standard strings
  date_str = utyear + '-' + utmonth + '-' + utday
  time_str = uthour + ':' + utminute + ':' + utsecond

  sxaddpar, header, 'TIMESYS', 'UTC', ' Coordinated Universal Time', after='EXTEND'

  sxaddpar, header, 'DATE-OBS', date_str, $
            ' [UTC] OBSERVATION DATE: CCYY-MM-DD', after='TIMESYS'
  sxaddpar, header, 'TIME-OBS', time_str, $
            ' [UTC] OBSERVATION TIME: HH:MM:SS', after='DATE-OBS'

  ; move the non-standard local time keywords to the end of the header
  sxdelpar, header, 'DATE_OBS'
  sxdelpar, header, 'TIME_OBS'

  carr = fix(tim2carr(date_str + ' ' + time_str, /dc))
  sxaddpar, header, 'CARR_ROT', carr[0], ' Carrington Rotation Number', 'END'

  sxaddpar, header, 'DATE_HST', hst_date_str, ' [HST] DATE OF OBSERVATION', 'END'
  sxaddpar, header, 'TIME_HST', hst_time_str, ' [HST] TIME OF OBSERVATION', 'END'
end
