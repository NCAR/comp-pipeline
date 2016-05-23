; docformat = 'rst'

; find the mean ND transmission values

@comp_constants_common

raw_basedir = '/hao/mlsodata1/Data/CoMP/raw'
process_basedir = '/hao/kaula1/Data/CoMP/process'
date = '20160516'

comp_initialize, date

flat_time = '111935'
dark_filename = date + '.112953.FTS'

wave_type = '1083'
wavelength = 1082.88
polstate = 'I+V'

if (wave_type eq '1074') then begin
  times = ['113101', '113304', '113506', '113709', '113911', '114114', '114316', '114519']
endif else begin
  times = ['113149', '113351', '113554', '113756', '113958', '114201', '114403', '114606']
endelse

n_files = n_elements(times)
files = strarr(n_files)
for f = 0L, n_files - 1L do begin
  files[f] = filepath(string(date, times[f], format='(%"%s.%s.FTS")'), $
                      subdir=date, root=raw_basedir)
endfor

n_beams = 2
n_nd_filters = 8
trans = fltarr(n_beams, n_nd_filters)
n_trans = lonarr(n_beams, n_nd_filters)

; use flat file to find upper_mask and lower_mask
fits_open, filepath('flat.fts', subdir=date, root=process_basedir), flat_fcb
fits_read, flat_fcb, flat_times, exten_no=flat_fcb.nextend - 2

fhrs = strmid(flat_time, 0, 2)
fmin = strmid(flat_time, 2, 2)
fsec = strmid(flat_time, 4, 2)
ftime = float(fhrs) + float(fmin) / 60. + float(fsec) / 60. / 60.
!null = min(abs(flat_times - fmin), min_index)
fits_read, flat_fcb, flat_data, flat_header, exten_no=min_index + 1L
fits_close, flat_fcb

; get parameters from FITS header                                                          
occulter1 = {x:sxpar(flat_header, 'OXCNTER1') - nx / 2, $
             y:sxpar(flat_header, 'OYCNTER1') - 1024 + ny / 2, $
             r:sxpar(flat_header, 'ORADIUS1')}
occulter2 = {x:sxpar(flat_header, 'OXCNTER2') - 1024 + nx / 2, $
             y:sxpar(flat_header, 'OYCNTER2') - ny / 2, $
             r:sxpar(flat_header, 'ORADIUS2')}
  
; field position                                                                           
field1 = {x:sxpar(flat_header, 'FXCNTER1') - nx / 2, $
          y:sxpar(flat_header, 'FYCNTER1') - 1024 + ny / 2, $
          r:sxpar(flat_header, 'FRADIUS1')}
field2 = {x:sxpar(flat_header, 'FXCNTER2') - 1024 + nx / 2, $
          y:sxpar(flat_header, 'FYCNTER2') - ny / 2, $
          r:sxpar(flat_header, 'FRADIUS2')}

; P angles of post
post_angle1 = sxpar(flat_header, 'POSTANG1')
post_angle2 = sxpar(flat_header, 'POSTANG2')

mask = comp_mask_1024(occulter1, occulter2, $
                      field1, field2, $
                      post_angle1, post_angle2, $
                      o_offset=0.0, f_offset=0.0)

x = rebin(reform(lindgen(1024), 1024, 1), 1024, 1024)
y = rebin(reform(lindgen(1024), 1, 1024), 1024, 1024)

m = 1.01
upper_mask = mask and (y gt m * x)
lower_mask = mask and (y le m * x)

mg_log, 'found masks...', /info

; use dark file to find dark data
fits_open, filepath(dark_filename, subdir=date, root=raw_basedir), dark_fcb
fits_read, dark_fcb, dark_data, exten_no=2  ; staying away from 1st ext
fits_close, dark_fcb

for f = 0L, n_files - 1L do begin
  mg_log, 'reading file %d/%d', f + 1, n_files, /info
  fits_open, files[f], fcb
  for e = 1L, fcb.nextend do begin
    fits_read, fcb, data, header, exten_no=e
    ext_polstate = sxpar(header, 'POLSTATE')
    ext_wavelength = sxpar(header, 'WAVELENG')
    if (ext_polstate eq polstate && abs(ext_wavelength - wavelength) lt 0.01) then begin
      nd_filter = sxpar(header, 'NDFILTER')
      beam = sxpar(header, 'BEAM')

      ; dark correct data
      data -= dark_data

      ; mask by data annulus from flat file to get science data and find mean
      if (beam lt 0L) then begin
        data_mean = total(data * upper_mask, /preserve_type) / total(upper_mask)
      endif else begin
        data_mean = total(data * lower_mask, /preserve_type) / total(lower_mask)
      endelse

      ; save result
      trans[beam gt 0L, nd_filter - 1L] += data_mean
      n_trans[beam gt 0L, nd_filter - 1L]++
    endif
  endfor
  fits_close, fcb
endfor

mean_trans = trans / n_trans

print, polstate, wavelength, format='(%"polarization state: %s, wavelength: %0.2f")'

print, 'Value', 'Ratio to ND 8', format='(%"%25s %25s")'
print, 'Upper left', 'Lower right', 'Upper left', 'Lower right', $
       format='(%"%12s %12s %12s %12s")'

underline = string(bytarr(12) + (byte('-'))[0])
print, underline, underline, underline, underline, $
       format='(%"%12s %12s %12s %12s")'

for n = 0L, n_nd_filters - 1L do begin
  print, mean_trans[0:1, n], mean_trans[0:1, n] / mean_trans[0:1, 7], $
         format='(%"%12.4f %12.4f %12.4f %12.4f")'
endfor

nd = [0.1, 0.3, 0.3, 1.0, 2.0, 3.0, 4.0, 0.0]
print, 10^(-nd), format='(8F8.4)'

end
