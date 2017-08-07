; docformat = 'rst'

;+
; Write an intermediate FITS file as it is given primary header, extension
; images, and extension headers.
;
; :Params:
;   primary_header : in, required, type=strarr(n_tags0)
;     primary header to write
;   images : in, required, type="fltarr(nx, ny, n_images)"
;     extension images
;   headers : in, required, type="strarr(n_tags, n_images)"
;     extension headers
;   wave_type : in, required, type=string
;     wave type, i.e., '1074', '1079', or '1083'
;   date : in, required, type=string
;     date in the form 'YYYYMMDD'
;   filename : in, required, type=string
;     level 0 filename
;   intermediate_type : in, required, type=string
;     string to add to filename in the format::
;
;       [date].[time].comp.[wave_type].[pols].[nwaves].[type].fts
;-
pro comp_write_intermediate, primary_header, images, headers, $
                             wave_type, date, $
                             filename, intermediate_type
  compile_opt strictarr

  @comp_config_common

  comp_inventory_header, headers, beam, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret

  n_wave = n_elements(uniq(wave, sort(wave)))
  _pol = ['I', strmid(pol, strlen(pol[0]) - 1, 1)]   ; take last char of I+Q, I-Q, I+U, etc.
  _pol = _pol[uniq(_pol, sort(_pol))]
  pol_tag = strlowcase(strjoin(_pol))

  eng_dir = filepath('', subdir=comp_decompose_date(date), root=engineering_dir)
  basename = string(file_basename(filename, '.FTS'), $
                    wave_type, pol_tag, n_wave, intermediate_type, $
                    format='(%"%s.comp.%s.%s.%d.%s.fts")')
  output_filename = filepath(basename, root=eng_dir)

  fits_open, output_filename, fcb, /write
  fits_write, fcb, 0.0, primary_header
  for e = 1L, n_elements(images[0, 0, *]) do begin
    ename = pol[e - 1] + ', ' + string(wave[e - 1], format='(f7.2)')
    fits_write, fcb, images[*, *, e - 1], headers[*, e - 1], extname=ename
  endfor
  fits_close, fcb
end
