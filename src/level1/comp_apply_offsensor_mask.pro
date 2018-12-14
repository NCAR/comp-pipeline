; docformat = 'rst'

;+
; Replace off-sensor pixels with NaNs.
;
; :Params:
;   images : in, out, required, type="fltarr(nx, ny, n_images)"
;     pass array of images as a named variable to be modified
;   offsensor_mask : in, required, type="bytarr(nx, ny)"
;     mask of pixels on the sensor: 1 for on-sensor, 0 for off-sensor
;
; :Keywords:
;   filename : in, optional, type=string
;     L0 filename to use to create a properly named engineering output of the
;     offsensor mask
;-
pro comp_apply_offsensor_mask, images, offsensor_mask, filename=filename
  compile_opt strictarr
  @comp_config_common

  save_offsensor_mask = 1B
  if (save_offsensor_mask && n_elements(filename) gt 0L) then begin
    offsensor_basename = file_basename(filename, '.FTS') + '.offsensor.sav'
    date = strmid(offsensor_basename, 0, 8)
    offsensor_filename = filepath(offsensor_basename, $
                                  subdir=comp_decompose_date(date), $
                                  root=engineering_dir)
    save, offsensor_mask, filename=offsensor_filename
  endif

  offsensor_indices = where(offsensor_mask ne 1.0, n_offsensor_pixels)
  dims = size(images, /dimensions)
  for i = 0L, dims[2] - 1L do begin
    x = images[*, *, i]
    x[offsensor_indices] = !values.f_nan
    images[*, *, i] = x
  endfor
end
