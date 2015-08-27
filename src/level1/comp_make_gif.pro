; docformat = 'rst'

;+
; Write a GIF file from the FITS file for CoMP.
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   filename : in, required, type=string
;     input FITS file
;   size : in, required, type=long
;     output square image length in pixels
;   label : in, required, type=string
;     text annotation to go in lower left
;   min : in, required, type=float
;     minimum in data for byte scaling
;   max : in, required, type=float
;     maximum in data for byte scaling
;
; :Author:
;   sitongia
;-
pro comp_make_gif, date_dir, filename, size, label, wave, min, max
  compile_opt strictarr

  newFilename = strmid(filename, 0, 35) + '.gif'

  ; read the FITS images
  fits_open, filename, fcbin 
  fits_read, fcbin, d, primary_header, /header_only, exten_no=0
  fits_read, fcbin, image, header, exten_no=1
  fits_close, fcbin

  ; mask
  comp_make_mask, date_dir, primary_header, mask

  image *= mask

  ; square root stretch
  if (wave eq '1083') then begin
    image = image ^ 0.3
  endif else begin
    image = sqrt(image)
  endelse

  top = 250
  image = bytscl(image, min=min, max=max, top=top)

  ; Resize the data
  s = size(image)
  if (size ne s[1]) then begin
    image = rebin(image, size, size)
  endif

  ; configure the device
  set_plot,'z'
  device, set_resolution=[size,size], set_colors=256, z_buffering=0, $
          decomposed=0
  loadct, 3, /silent

  tv, image

  ; locations and such
  xdim = size
  ydim = size
  ccol = 255
  xstrt = xdim - 2 * xdim / 512
  strt = ydim - 10 * ydim / 512
  Bsz = 1.4
  Lsz = 1.15
  Dsz = 1.0
  del = 10
  date = sxpar(primary_header, 'DATE-OBS')
  time = sxpar(primary_header, 'TIME-OBS')

  xyouts, 5, 25, 'Scaling:' + string(min, max, format='(F4.1, " to ", F4.1)'), $
          charsize=Lsz, /device, color=ccol
  if (wave eq '1083') then begin
    xyouts, 5, 5, wave + ' -- im^0.3 ' + label, charsize=Lsz, /device, color=ccol
  endif else begin
    xyouts, 5, 5, wave + ' -- sqrt ' + label, charsize=Lsz, /device, color=ccol
  endelse

  xyouts, 5, strt - del * 1, 'MLSO/HAO/CoMP', charsize=Bsz, /device, color=ccol
  xyouts, xstrt, strt - del * 1, date, charsize=Lsz, /device, color=ccol, $
          alignment=1.0
  xyouts, xstrt, strt - del * 3, time +' UTC', charsize=Lsz, /device, $
          color=ccol, alignment=1.0
  xyouts, xdim / 2 - 1, ydim - 10 * ydim / 512, 'North', charsize=Dsz, $
          /device, color=ccol, alignment=0.5
  xyouts, 10, ydim / 2 - 1, 'East', charsize=Dsz, /device, color=ccol, $
          alignment=0.5, orientation=90
  xyouts, xstrt, ydim / 2 - 1, 'West', charsize=Dsz, /device, color=ccol, $
          alignment=0.5, orientation=90

  colorbar2, position=[0.70, 0.02, 0.98, 0.06], range=[min, max + 0.5], $
             divisions=5, charsize=0.6

  write_gif, newFilename, tvrd()
end
