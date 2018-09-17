; docformat = 'rst'

;+
; Scattering within the CoMP birefringent filter causes stray light to be
; superimposed on the CoMP images. This is evident by a distribution of light
; present in areas that should be dark, behind the occulting disk and outside
; the CoMP field-of-view. This routine fits the stray light outside the CoMP
; field-of-view with a polynomial and uses the fit to interpolate the stray
; light over the comp field-of-view, which is subrtacted from the input image.
;
; :Examples:
;   For example, call like::
;
;     comp_fix_stray_light, image, header, fit
;
; :Uses:
;   comp_sfit, comp_eval_surf
;
; :Returns:
;   image is returned with the stray light fit subtracted from it.
;
; :Params:
;   image : in, out, required, type="fltarr(1024, 1024)"
;     the 1024x1024 CoMP image to be stray light corrected
;   flat_header : in, required, type=strarr
;     header from a flat image containing occulter and field stop positions
;   fit : out, optional, type="fltarr(1024, 1024)"
;     the fit which was subtracted from image
;
; :Author:
;   MLSO Software Team
;
; :History:
;   10/8/14 - POLYWARP was replaced by COMP_SFIT to increase speed
;   see git log for recent changes
;-
pro comp_fix_stray_light, image, flat_header, fit, coefficients=kx, max_degree=max_degree
  compile_opt strictarr

  dims = size(image, /dimensions)   ; get image size
  nx = dims[0]
  ny = dims[1]

  ; create arrays of x coordinate and y coordinate

  x = rebin(findgen(nx), nx, ny) - float(nx) * 0.5
  y = rebin(reform(findgen(ny), 1, ny), nx, ny) - float(ny) * 0.5

  ; create mask with cutoff inside of occulter and outside of field mask

  image_mask = comp_annulus_1024(flat_header, o_offset=-20.0, f_offset=+28.0, $
                                 /uncorrected)

  ; Identify pixels to use. Omit pixels in dark corners of flat image.
  ; the version of this line which does not omit the corners of the image is:
  ; good=where(image_mask eq 0.0,count)

  good = where(image_mask eq 0.0 and y lt (- x + 600.0) and y gt (- x - 600.0), $
               count)

  ; fit stray light outside of field-of-view

  data = fltarr(3, count, /nozero)
  data[0, *] = x[good]
  data[1, *] = y[good]
  data[2, *] = image[good]

  ndeg = 2L   ; degree of polynomial fit
  kx = comp_sfit(data, ndeg, /irregular, max_degree=max_degree)

  ; compute fit from coefficients and subtract it from input image
  fit = comp_eval_surf(transpose(kx), reform(x[*, 0]), reform(y[0, *]))

  image -= fit
end


; main-level example program

date = '20180131'
filename = '/hao/mahidata1/Data/CoMP/raw/20180131/20180131.083106.FTS'
ext = 12
config_filename = filepath('comp.mgalloy.mahi.latest.cfg', $
                           subdir=['..', '..', 'config'], $
                           root=mg_src_root())

comp_initialize, date
comp_configuration, config_filename=config_filename

comp_read_data, filename, images, headers, header0
comp_inventory_header, headers, beam, wave, pol, type, exposure, $
                       cover, cal_pol, cal_ret

time = comp_extract_time(headers)
dark = comp_dark_interp(date, time, exposure)
comp_read_flats, date, wave, beam, time, flat, flat_header, flat_waves, $
                 flat_names, flat_expose, flat_extensions=flat_extensions, $
                 flat_found=flat_found, normalize=normalize

image = images[*, *, ext - 1]
header = headers[*, ext - 1]

iflat = where(abs(flat_waves) eq wave[ext - 1] and sgn(flat_waves) eq beam[ext - 1])
iflat = iflat[0]

image -= dark
image = comp_fixrock(image, 0.030)
image = comp_fix_image(image)

comp_fix_stray_light, image, flat_header[*, iflat], fit, coefficients=kx

pt1 = [0.0, 128.0]
max_value = max(abs(fit), max_index)
if (fit[max_index] gt 0L) then begin
  pt2 = [max(fit), 255.0]
endif else begin
  pt2 = [min(fit), 0.0]
endelse

window, xsize=1024, ysize=1024, /free
device, get_decomposed=original_decomposed
device, decomposed=0
loadct, 70
tv, (pt2[1] - pt1[1]) / (pt2[0] - pt1[0]) * (fit - pt1[0]) + pt1[1]
device, decomposed=original_decomposed

fit_moment = moment(fit)
print, strjoin(strtrim(fit_moment, 2), ', '), format='(%"Fit moments: %s")'
print, min(fit, max=max_value), max_value, format='(%"min: %0.3f, max: %0.3f")'

end
