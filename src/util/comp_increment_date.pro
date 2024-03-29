; docformat = 'rst'

function comp_increment_date, date
  compile_opt strictarr

  date_parts = long(comp_decompose_date(date))
  jd = julday(date_parts[1], date_parts[2] + 1L, date_parts[0])
  return, string(jd, '(C(CYI4.4,CMOI2.2,CDI2.2))')
end


; main-level example program

dates = ['20111231', '20130605', '20200228']
fmt = '(%"%s -> %s -> %s")'
for d = 0L, n_elements(dates) - 1L do begin
  print, dates[d], $
         comp_increment_date(dates[d]), $
         comp_increment_date(comp_increment_date(dates[d])), $
         format=fmt
endfor

end
