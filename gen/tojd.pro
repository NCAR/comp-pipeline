function tojd, day, month, year, hours, mins, secs
;
;   TOJD - SUBROUTINE TO CONVERT FROM UT DATE TO JULIAN DATE
; $Id: tojd.pro,v 1.2 2000/08/23 23:01:24 jgraham Exp $
;

         idays=[0,31,59,90,120,151,181,212,243,273,304,334]
;
;   THE FOLLOWING CALCULATIONS DEAL WITH TIME.
;   THE EPOCH IS 1900 JAN 0.5 UT = JD 2415020.0
;
   century=fix( (year-1)/100 )
   jul_day = double(1721425.d0 + 365.d0*(double(year)-1.) + fix( (year-1)/4 ) $
      - century + fix(century/4) )
;
;   UTDA IS THE GMT FROM JAN 0.0 TO THE PRESENT (DAYS)
;
   utda = double( double(idays(month-1)) + double(day) + double(hours)/24.d0 $
    +double(mins)/1440.d0 +double(secs)/86400.d0 )
;
;   IF THIS YEAR IS A LEAP YEAR, AND IT IS LATER THAN FEB 29.0,
;   ADD A DAY
;

; Looks like you want to process entire arrays - okay then

   ; If year divisible by 4 then leap year
   good = Where(year MOD 4 EQ 0 AND month GT 2, count)
   if count gt 0 then utda(good)=utda(good)+1.d0
   ; But if year divisible by 100 not leap year
   good = Where(year MOD 100 EQ 0 AND month GT 2, count)
   if count gt 0 then utda(good)=utda(good)-1.d0
   ; Unless year also divisible by 400
   good = Where(year MOD 400 EQ 0 AND month GT 2, count)
   if count gt 0 then utda(good)=utda(good)+1.d0

;
   jd = jul_day + utda -.5d0
   return, jd
   end
