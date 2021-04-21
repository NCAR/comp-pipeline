; docformat = 'rst'

;+
; Write a GIF file from the FITS file for CoMP.
;
; :Uses:
;   colorbar2, sxpar
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
;  mask = comp_l1_mask(date_dir, primary_header)
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
        if (keyword_set(background)) then begin
          min = dispmin1079bkg
          max = dispmax1079bkg
          dispexp = dispexp1079bkg
        endif else begin
          min = dispmin1079
          max = dispmax1079
          dispexp = dispexp1079
        endelse
      end
    '1083': begin
        if (keyword_set(background)) then begin
          min = dispmin1083bkg
          max = dispmax1083bkg
          dispexp = dispexp1083bkg
        endif else begin
          min = dispmin1083
          max = dispmax1083
          dispexp = dispexp1083
        endelse
      end
  endcase

  ; configure the device
  original_device = !d.name

  set_plot, 'Z'
  device, set_resolution=[size, size], $
          z_buffering=0, $
          decomposed=0, $
          set_pixel_depth=8, $
          set_colors=256

  loadct, 3, /silent, ncolors=256 - n_annotation_colors

  if (keyword_set(background)) then begin
    bad1_col = 251
    tvlct, 255, 255, 0, bad1_ocol
    bad2_col = 252
    tvlct, 0, 255, 255 bad2_ocol
  endif

  ocol = 253
  tvlct, 0, 200, 255, ocol
  fcol = 254
  tvlct, 0, 255, 0, fcol
  ccol = 255
  tvlct, 255, 255, 255, ccol

  n_annotation_colors = 3
  n_threshold_colors = 2
  if (keyword_set(background)) then begin
    top = 255 - n_annotation_colors - n_threshold_colors
  endif else begin
    top = 255 - n_annotation_colors
  endelse

  if (keyword_set(background)) then begin
    over1_indices = where(image gt max, n_over1)
    over2_indices = where(image gt gbu_background_threshold, n_over2)
  endif

  image = image ^ dispexp
  image = bytscl(image, min=min, max=max, top=top)

  if (keyword_set(background)) then begin
    image[over1_indices] = bad1_col
    image[over2_indices] = bad2_col
  endif

  ; resize the data
  s = size(image)
  if (size ne s[1]) then begin
    image = rebin(image, size, size)
  endif

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

  xyouts, 5, 25, 'Scaling:' + string(min, max, format='(F0.1, " to ", F0.1)'), $
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

  ; this had a range=[min, max + 0.5] originally, not sure why
  colorbar2, position=[0.70, 0.02, 0.98, 0.06], range=[min, max], $
             divisions=5, charsize=0.6, font=font, ncolors=top + 1L, $
             format='(F0.1)'

  oradius = sxpar(primary_header, 'ORADIUS')
  oxcenter = sxpar(primary_header, 'CRPIX1') - 1.0
  oycenter = sxpar(primary_header, 'CRPIX2') - 1.0

  theta = findgen(360) * !dtor

  ; occulter center and outline
  x = oradius * cos(theta) + oxcenter
  y = oradius * sin(theta) + oycenter
  plots, x, y, /device, color=ocol, thick=2
  plots, [oxcenter], [oycenter], /device, color=ocol, psym=1

  if (keyword_set(background)) then begin
    fradius = sxpar(primary_header, 'FRADIUS')
    fxcenter = sxpar(primary_header, 'FRPIX1') - 1.0
    fycenter = sxpar(primary_header, 'FRPIX2') - 1.0

    post_angle = sxpar(primary_header, 'POSTPANG')
    p_angle = sxpar(primary_header, 'SOLAR_P0')

    ; field center and outline
    x = fradius * cos(theta) + fxcenter
    y = fradius * sin(theta) + fycenter
    plots, x, y, /device, color=fcol, thick=1
    plots, [fxcenter], [fycenter], /device, color=fcol, psym=1

    ; post
    r = (oradius + fradius) / 2.0

    ; convert from position angle (0 degrees up) to mathematical convention
    ; in radians
    pa = (post_angle + 90.0) * !dtor
    plots, [r * cos(pa) + oxcenter], [r * sin(pa) + oycenter], $
           /device, color=ocol, psym=1
  endif

  im = tvrd(true=0)
  tvlct, r, g, b, /get
  set_plot, original_device

  write_gif, filename, im, r, g, b
end


; main-level example program

date = '20180103'
comp_initialize, date

process_basedir = '/hao/mahidata1/Data/CoMP/process.new-olddist-strayplain'
filename = filepath('20180104.023830.comp.1074.iv.5.bkg.fts.gz', $
                    subdir=[date, 'level1'], $
                    root=process_basedir)
fits_open, filename, fcb
fits_read, fcb, data, primary_header, exten_no=0
fits_read, fcb, intensity, header, exten_no=2
fits_close, fcb

comp_make_gif, date, intensity, primary_header, 'test.gif', $
               620, 'Background', '1074', $
               /background

end
