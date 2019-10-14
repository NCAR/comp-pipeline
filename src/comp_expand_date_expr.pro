; docformat = 'rst'

function comp_expand_date_expr_range, start_date, end_date, $
                                      count=n_days, $
                                      error=error
  compile_opt strictarr

  error = 0L

  start_parts = comp_decompose_date(start_date)
  start_jd = julday(start_parts[1], start_parts[2], start_parts[0], 0, 0, 0.0D)
  end_parts = comp_decompose_date(end_date)
  end_jd = julday(end_parts[1], end_parts[2], end_parts[0], 0, 0, 0.0D)

  n_days = long(end_jd - start_jd)
  days = strarr(n_days)

  for d = 0L, n_days - 1L do begin
    caldat, start_jd + d, month, day, year
    days[d] = string(year, month, day, format='(%"%04d%02d%02d")')
  endfor

  return, days
end


;+
; Expand a date expression into an array of string dates.
;
; :Returns:
;   `strarr`, `!null` if no dates
;
; :Params:
;   date_expr : in, required, type=string
;     string date expression separating expressions with commas and ranges with
;     dashes, such as::
;
;       20190101-20190201,20190513
;
;     start of a range is inclusive, end of a range is exclusive
;-
function comp_expand_date_expr, date_expr, error=error, count=count
  compile_opt strictarr

  dates_list = list()

  tokens = strsplit(date_expr, ',', /extract, count=n_tokens)
  for t = 0L, n_tokens - 1L do begin
    if (strpos(tokens[t], '-') lt 0L) then begin
      dates_list->add, tokens[t]
    endif else begin
      endpts = strsplit(tokens[t], '-', /extract, count=n_endpts)
      dates_list->add, comp_expand_date_expr_range(endpts[0], endpts[1]), /extract
    endelse
  endfor

  dates = dates_list->toArray()
  obj_destroy, dates_list

  count = n_elements(dates)

  return, dates
end


; main-level example program

expr = '20160201-20160301,20190513'

dates = comp_expand_date_expr(expr)
print, dates

end
