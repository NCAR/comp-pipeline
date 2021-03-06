; docformat = 'rst'


;+
; Expand a range of days expressed as a start and end date as an array of
; dates.
;
; :Returns:
;   strarr
;
; :Params:
;   start_date : in, required, type=string
;     start date in the form YYYYMMDD
;   end_date : in, required, type=string
;     end date in the form YYYYMMDD
;
; :Keywords:
;   count : out, optional, type=long
;     set to a named variable to retrieve the number of days in the range
;
; :Author:
;   MLSO Software Team
;-
function comp_verify_dates_expandrange, start_date, end_date, count=n_days
  compile_opt strictarr

  start_parts = comp_decompose_date(start_date)
  start_jd = julday(start_parts[1], start_parts[2], start_parts[0], 0, 0, 0.0D)
  end_parts = comp_decompose_date(end_date)
  end_jd = julday(end_parts[1], end_parts[2], end_parts[0], 0, 0, 0.0D)

  n_days = long(end_jd - start_jd) + 1L
  days = strarr(n_days)

  for d = 0L, n_days - 1L do begin
    caldat, start_jd + d, month, day, year
    days[d] = string(year, month, day, format='(%"%04d%02d%02d")')
  endfor

  return, days
end


;+
; Take a date expression and call `COMP_VERIFY` as required on the individual
; dates.
;
; Exits IDL with exit status equal to the number of failed days.
;
; :Params:
;   date_expression : in, required, type=string
;     expression of dates in the form YYYYMMDD, such as::
;
;       20170401-20170414,20170418
;
; :Keywords:
;   config_filename, in, optional, type=string
;     configuration filename to use, default is `comp.cfg` in the `src`
;     directory
;
; :Author:
;   MLSO Software Team
;-
pro comp_verify_dates, date_expression, config_filename=config_filename
  compile_opt strictarr
  on_error, 2

  mg_log, name='comp/verify', logger=logger
  logger->setProperty, format='%(time)s %(levelshortname)s: %(message)s'

  ranges = strsplit(date_expression, ',', /extract, count=n_ranges)

  failed_days = list()

  divider = string(bytarr(35) + (byte('-'))[0])

  for r = 0L, n_ranges - 1L do begin
    endpts = strsplit(ranges[r], '-', /extract, count=n_endpts)
    case n_endpts of
      0: ; missing range expression, just skip
      1: begin
          comp_verify, endpts[0], config_filename=config_filename, status=status
          if (status ne 0L) then failed_days->add, endpts[0]
          mg_log, divider, name='comp/verify', /info
        end
      2: begin
          dates = comp_verify_dates_expandrange(endpts[0], endpts[1], $
                                                count=n_dates)
          for d = 0L, n_dates - 1L do begin
            comp_verify, dates[d], config_filename=config_filename, status=status
            if (status ne 0L) then failed_days->add, dates[d]
            mg_log, divider, name='comp/verify', /info 
         endfor
        end
      else: message, 'invalid date expression syntax'
    endcase
  endfor

  n_failed_days = failed_days->count()
  if (n_failed_days gt 0L) then begin
    mg_log, 'failed days: %s', strjoin(failed_days->toArray(), ', '), $
            name='comp/verify', /info
  endif else begin
    mg_log, 'no failed days', name='comp/verify', /info
  endelse

  obj_destroy, failed_days
  exit, status=n_failed_days
end
