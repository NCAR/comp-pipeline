; docformat = 'rst'

;+
; Write a GIF file from the FITS file for CoMP.
;
; :Uses:
;   comp_make_mask, colorbar2, sxpar
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
; :Keywords:
;   background : in, optional, type=boolean
;     set to produce a GIF of background data
;
; :Author:
;   MLSO Software Team
;-
pro comp_make_gif, date_dir, image, primary_header, filename, size, label, $
                   wave, $
                   background=background
  compile_opt strictarr
  @comp_constants_common

  ; not masking GIFs now
  ; mask
;  comp_make_mask, date_dir, primary_header, mask
;  image *= mask

  ; exponent stretch
  case wave of
    '1074': begin
        if (keyword_set(background)) then begin
          min = dispmin1074bkg
          max = dispmax1074bkg
          dispexp = dispexp1074bkg
        endif else begin
          min = dispmin1074
          max = dispmax1074
          dispexp = dispexp1074
        endelse
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

  ; resize the data
  s = size(image)
  if (size ne s[1]) then begin
    image = rebin(image, size, size)
  endif

  ; configure the device
  original_device = !d.name
  set_plot, 'Z'
  device, set_resolution=[size, size], z_buffering=0, decomposed=0

  loadct, 3, /silent, ncolors=253

  ocol = 253
  tvlct, 255, 255, 0, ocol
  fcol = 254
  tvlct, 0, 255, 0, fcol
  ccol = 255
  tvlct, 255, 255, 255, ccol

  tv, image

  ; locations and such
  xdim = size
  ydim = size

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
  xyouts, 5, 5, string(wave, dispexp, label, format='(%"%s -- im^%0.1f %s")'), $
          charsize=Lsz, /device, $
          color=ccol, font=font

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
             divisions=5, charsize=0.6, font=font, ncolors=top + 1L, $
             format='(F0.1)'

  if (keyword_set(background)) then begin
    oradius = sxpar(primary_header, 'ORADIUS')
    oxcenter = sxpar(primary_header, 'CRPIX1') - 1.0
    oycenter = sxpar(primary_header, 'CRPIX2') - 1.0

    fradius = sxpar(primary_header, 'FRADIUS')
    fxcenter = sxpar(primary_header, 'FRPIX1') - 1.0
    fycenter = sxpar(primary_header, 'FRPIX2') - 1.0

    post_angle = sxpar(primary_header, 'POSTPANG')
    p_angle = sxpar(primary_header, 'SOLAR_P0')

    theta = findgen(360) * !dtor

    ; occulter center and outline
    x = oradius * cos(theta) + oxcenter
    y = oradius * sin(theta) + oycenter
    plots, x, y, /device, color=ocol
    plots, [oxcenter], [oycenter], /device, color=ocol, psym=1

    ; field center and outline
    x = fradius * cos(theta) + fxcenter
    y = fradius * sin(theta) + fycenter
    plots, x, y, /device, color=fcol
    plots, [fxcenter], [fycenter], /device, color=fcol, psym=1

    ; post
    r = (oradius + fradius) / 2.0

    ; convert from position angle (0 degrees up) to mathematical convention
    ; in radians
    pa = (post_angle + 90.0) * !dtor
    plots, [r * cos(pa) + oxcenter], [r * sin(pa) + oycenter], $
           /device, color=ocol, psym=1
  endif

  im = tvrd()
  tvlct, r, g, b, /get
  set_plot, original_device

  write_gif, filename, im, r, g, b
end
