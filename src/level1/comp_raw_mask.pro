; docformat='rst'

;+
; Quick and dirty program to create a mask for comp raw data. Works by
; thresholding the flats.
;
; :Uses:
;   comp_inventory_header, comp_extract_time, comp_read_flats
;
; :Returns:
;   mask image, array sized nx * ny
;
; :Params:
;   date_dir : in, required, type=string
;     name of date directory containing the files corresponding to the headers
;   headers : in, required, type="strarr(ntags, nimages)"
;     extension headers for a file
;
; :Keywords:
;   upper_left_mask :  out, optional, type="fltarr(nx, ny)"
;     mask of the upper left portion of the image
;   lower_right_mask :  out, optional, type="fltarr(nx, ny)"
;     mask of the lower right portion of the image
;   threshold: in, optional, type=float, default=15
;     value for thresholding flats
;
; :Author:
;   Joseph Plowman
;-
function comp_raw_mask, date_dir, headers, $
                        upper_left_mask=ul_mask, lower_right_mask=lr_mask, $
                        threshold=threshold
  compile_opt strictarr

  _threshold = n_elements(threshold) eq 0L ? 20 : threshold

  ; get the flat
  comp_inventory_header, headers, beam, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret
  time = comp_extract_time(headers, day, month, year, hours, mins, secs)
  comp_read_flats, date_dir, wave, beam, time, flat, flat_header, flat_waves, $
                   flat_names, flat_expose

  ; wavelength average the flats and apply a median smoothing
  flatmean = median(mean(flat, dimension=3), 3)
  
  ; check to see if values outside the annulus have been set to a constant:
  flatmean_med = median(flatmean)
  medmask = flatmean eq flatmean_med
  if(total(medmask) gt 0.5*n_elements(flatmean)) then flatmean[where(medmask)] = 0.0  

  nx = n_elements(flatmean[*, 0])
  ny = n_elements(flatmean[0, *])

  xa = dindgen(nx) # (1.0 + dblarr(nx))
  ya = transpose(xa)

  ; mask out everywhere flatmean is less than the threshold
  mask = flatmean gt _threshold

  ; chop the image in half diagonally (with offset), padding by pad, to make
  ; the two sub-masks
  if (n_elements(offset) eq 0L) then offset = -3
  if (n_elements(pad) eq 0L) then pad = 8
  if (n_elements(erodenum) eq 0L) then erodenum = 5

  ul_mask = erode(mask * (xa lt ya + offset - pad), $
                replicate(1, erodenum, erodenum))
  lr_mask = erode(mask * (xa gt ya + offset + pad), $
                replicate(1, erodenum, erodenum))
  ul_mask[265:325, 400:600] = 0
  lr_mask[680:740, 0:100] = 0

  return, ul_mask or lr_mask
end
