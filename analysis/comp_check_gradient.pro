; docformat = 'rst'

pro comp_check_gradient, filename, l1_dir
  compile_opt strictarr

  ; find decimal hour in HST
  time_string = strmid(file_basename(filename), 9, 6)
  hr = strmid(time_string, 0, 2)
  min = strmid(time_string, 2, 2)
  sec = strmid(time_string, 4, 2)
  time = long(hr) + long(min) / 60.0 + long(sec) / 60.0 / 60.0

  ; read and inventory data
  comp_read_data, filename, images, headers, primary_header
  comp_inventory_header, headers, $
                         file_beam, file_wave, file_pol, file_type, file_expose

  ; skip first extension
  file_beam = file_beam[1:*]
  file_wave = file_wave[1:*]
  file_pol = file_pol[1:*]

  images = images[*, *, 1:*]
  headers = headers[*, 1:*]

  ubeam = file_beam[uniq(file_beam, sort(file_beam))]
  uwave = file_wave[uniq(file_wave, sort(file_wave))]
  upol = file_pol[uniq(file_pol, sort(file_pol))]

  nw = n_elements(uwave)
  np = n_elements(upol)

  ; open dark file
  dark_filename = filepath('dark.fts', root=l1_dir)
  fits_open, dark_filename, dark_fcb

  ; read dark exposures and times
  fits_read, dark_fcb, dark_exposures, dark_exposures_hdu, extname='Exposure'
  fits_read, dark_fcb, dark_times, dark_time_hdu, extname='Time'

  ; find dark matching exposure and closest in time
  dark_exp_ind = where(dark_exposures eq file_expose[0], count)
  !null = min(abs(dark_times[dark_exp_ind] - time), index)
  fits_read, dark_fcb, dark, dark_hdu, exten_no=dark_exp_ind[index] + 1

  ; close dark file
  fits_close, dark_fcb

  nx = 128 + 64
  ny = 128 + 64
  for b = 0L, n_elements(ubeam) - 1L do begin
    title = string(ubeam[b], $
                   strjoin(string(uwave, format='(F0.2)'), ','), $
                   strjoin(upol, ', '), $
                   format='(%"beam %d [rows: %s] [cols: %s]")')
    window, xsize=nx * np, ysize=ny * nw, /free, title=title
    for w = 0L, n_elements(uwave) - 1L do begin
      for p = 0L, n_elements(upol) - 1L do begin

        ; find data extensions matching requested wave/beam/pol
        ind = where(uwave[w] eq file_wave $
                      and ubeam[b] eq file_beam $
                      and upol[p] eq file_pol, $
                    count)
        if (count ne 2) then begin
          message, string(count, format='(%"found %d matching extensions")')
        endif

        ;print, ubeam[b], uwave[w], upol[p], ind + 1, $
        ;format='(%"for beam %d, wave %0.2f, and pol %s, found exts: %d and %d")'
        raw1 = images[*, *, ind[0]]
        raw2 = images[*, *, ind[1]]

        im1 = raw1 - dark
        im2 = raw2 - dark

        im1 = comp_fixrock(im1, 0.030)
        im2 = comp_fixrock(im2, 0.030)

        im1 = comp_fix_image(im1)
        im2 = comp_fix_image(im2)
        diff = bytscl(im2 - im1, min=-100.0, max=100.0)
        diff = frebin(diff, nx, ny)
        ;diff = congrid(diff, nx, ny)
        tv, diff, np * w + p
      endfor
    endfor
  endfor
end


; main-level example

server = 'mahidata1'
date = '20180130'
time = '123540'

filename = string(server, date, date, time, $
                  format='(%"/hao/%s/Data/CoMP/raw/%s/%s.%s.FTS")')
l1_dir = string(server, date, $
                format='(%"/hao/%s/Data/CoMP/process/%s/level1")')

comp_initialize, date

comp_check_gradient, filename, l1_dir

end
