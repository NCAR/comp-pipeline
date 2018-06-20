; docformat = 'rst'

pro comp_fix_transmission, in_filename, out_filename
  compile_opt strictarr

  openr, in_lun, in_filename, /get_lun
  openw, out_lun, out_filename, /get_lun

  n_lines = file_lines(in_filename)
  line = ''
  for i = 0L, n_lines - 1L do begin
    readf, in_lun, line
    tokens = strsplit(line, ',', /extract)
    trans = float(tokens[3])
    trans *= 84.0 / comp_transmission(tokens[0])
    tokens[3] = string(trans, format='(%"%0.6f")')

    printf, out_lun, strjoin(tokens, ', ')
  endfor

  free_lun, in_lun
  free_lun, out_lun
end


; main-level example program

comp_fix_transmission, 'flat-medians-uncorrected.csv', $
                       'flat-medians-corrected.csv'

end
