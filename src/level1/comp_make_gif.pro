; docformat = 'rst'

;+
; Write a GIF file from the FITS file for CoMP.
;
; :Uses:
;   comp_make_mask, colorbar2, fits_open, fits_read, fits_close, sxpar
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   image : in, required, type="fltarr(620, 620)"
;     image to make a GIF of
;   primary_header : in, required, type=strarr
;     primary header corresponding to image data
;   filename : in, required, type=string
;     filename of output GIF file
;   size : in, required, type=long
;     output square image length in pixels
;   label : in, required, type=string
;     text annotation to go in lower left
;   wave : in, required, type=string
;     wave type, '1074', '1079', or '1083'
;
; :Author:
;   sitongia
;-
pro comp_make_gif, date_dir, image, primary_header, filename, size, label, $
                   wave
  compile_opt strictarr
  @comp_constants_common

  ; mask
  comp_make_mask, date_dir, primary_header, mask

  image *= mask

  ; square root stretch
  case wave of
    '1074': begin
        min = dispmin1074
        max = dispmax1074
        dispexp = dispexp1074
      end
    '1079': begin
        min = dispmin1079
        max = dispmax1079
        dispexp = dispexp1079
      end
    '1083': begin
        min = dispmin1083
        max = dispmax1083
        dispexp = dispexp1083
      end
  endcase

  image = image ^ dispexp

  top = 250
  image = bytscl(image, min=min, max=max, top=top)

  ; Resize the data
  s = size(image)
  if (size ne s[1]) then begin
    image = rebin(image, size, size)
  endif

  ; configure the device
  set_plot, 'z'
  device, set_resolution=[size, size], set_colors=256, z_buffering=0, $
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
  font = -1   ; use device fonts
  date = sxpar(primary_header, 'DATE-OBS')
  time = sxpar(primary_header, 'TIME-OBS')

  xyouts, 5, 25, 'Scaling:' + string(min, max, format='(F4.1, " to ", F4.1)'), $
          charsize=Lsz, /device, color=ccol, font=font
  if (wave eq '1083') then begin
    xyouts, 5, 5, wave + ' -- im^0.3 ' + label, charsize=Lsz, /device, $
            color=ccol, font=font
  endif else begin
    xyouts, 5, 5, wave + ' -- sqrt ' + label, charsize=Lsz, /device, $
            color=ccol, font=font
  endelse

  xyouts, 5, strt - del * 1, 'MLSO/HAO/CoMP', charsize=Bsz, /device, color=ccol, $
          font=font
  xyouts, xstrt, strt - del * 1, date, charsize=Lsz, /device, color=ccol, $
          alignment=1.0, font=font
  xyouts, xstrt, strt - del * 3, time +' UTC', charsize=Lsz, /device, $
          color=ccol, alignment=1.0, font=font
  xyouts, xdim / 2 - 1, ydim - 10 * ydim / 512, 'North', charsize=Dsz, $
          /device, color=ccol, alignment=0.5, font=font
  xyouts, 10, ydim / 2 - 1, 'East', charsize=Dsz, /device, color=ccol, $
          alignment=0.5, orientation=90, font=font
  xyouts, xstrt, ydim / 2 - 1, 'West', charsize=Dsz, /device, color=ccol, $
          alignment=0.5, orientation=90, font=font

  colorbar2, position=[0.70, 0.02, 0.98, 0.06], range=[min, max + 0.5], $
             divisions=5, charsize=0.6, font=font

  write_gif, filename, tvrd()
end
