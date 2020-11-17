; docformat = 'rst'


pro comp_process_distortion_display, im1, im2, alpha=alpha, range=range, $
                                     _extra=e
  compile_opt strictarr

  _alpha = n_elements(alpha) eq 0L ? 0.5 : alpha
  _range = n_elements(range) eq 0L ? [0.0, 1000.0] : range

  im = _alpha * im1 + (1.0 - _alpha) * im2

  dims = size(im1, /dimensions)
  window, xsize=3 * dims[0], ysize=dims[1], /free, _extra=e
  tv, bytscl(im1, _range[0], _range[1]), 0, 0
  tv, bytscl(im2, _range[0], _range[1]), 1*dims[0], 0
  tv, bytscl(im,  _range[0], _range[1]), 2*dims[0], 0
end

 
; main-level example program

; 20180330.123121.FTS ext 1, beam -1
; date = '20180330'
; time = '123121'
; ext = 1
; flat_date = date
; flat_ext = 512   ; 12:21:16
; dark_date = date
; dark_ext = 13    ; 12:30:07
; range = [0.0, 100.0]
; local_raw_basedir = '/hao/mahidata1/Data/CoMP/raw'

; 20161107.093403.FTS, ext 1, beam -1, 1074.62 nm
date = '20161107'
time = '093403'
ext = 1
flat_date = '20161106'
flat_ext = 6
dark_date = '20161107'
dark_ext = 1
range = [0.0, 100.0]
local_raw_basedir = '/hao/dawn/Data/CoMP/raw'

config_filename = filepath('comp.distortion.cfg', $
                           subdir=['..', 'config'], $
                           root=mg_src_root())

@comp_config_common
@comp_constants_common

comp_initialize, date
comp_configuration, config_filename=config_filename

flat_filename = filepath(string(flat_date, format='(%"%s.comp.flat.fts")'), $
                         subdir=[flat_date, 'level1'], $
                         root=process_basedir)

fits_open, flat_filename, flat_file_fcb
fits_read, flat_file_fcb, flat, flat_header, exten_no=flat_ext
fits_close, flat_file_fcb

; fill flats
mask_full_fill = comp_annulus_1024(flat_header, $
                                   o_offset=1.0, f_offset=-1.0, $
                                   /uncorrected)

good_pixels = where(mask_full_fill eq 1.0, n_good_pixels, $
                    complement=bad_pixels, ncomplement=n_bad_pixels)
medflat = median(flat[good_pixels])
flat[bad_pixels] = medflat

dark_filename = filepath(string(dark_date, format='(%"%s.comp.dark.fts")'), $
                         subdir=[dark_date, 'level1'], $
                         root=process_basedir)

fits_open, dark_filename, dark_file_fcb
fits_read, dark_file_fcb, dark, exten_no=dark_ext
fits_close, dark_file_fcb

data_filename = filepath(string(date, time, format='(%"%s.%s.FTS")'), $
                         subdir=[date], $
                         root=local_raw_basedir)

fits_open, data_filename, fcb
fits_read, fcb, im, exten_no=ext
fits_close, fcb

im = im - dark

im  = comp_fixrock(temporary(im), 0.030)
im  = comp_fix_image(temporary(im))

im = im / flat

restore, filename=hot_file, /verbose
im = comp_fix_hot(temporary(im), hot=hot, adjacent=adjacent)

im1 = comp_extract1(im)
im2 = comp_extract2(im)

; apply old distortion coefficients
old_dist_im1 = comp_apply_distortion_coeffs(im1, distortion_coeffs[0])
old_dist_im2 = comp_apply_distortion_coeffs(im2, distortion_coeffs[1])

; apply new distortion
restore, filename=filepath(distortion_coeffs_file, root=binary_dir), /verbose
new_dist_im1 = comp_apply_distortion_file(im1, dx1_c, dy1_c)
new_dist_im2 = comp_apply_distortion_file(im2, dx2_c, dy2_c)

save_filename = filepath(string(date, time, format='(%"%s.%s.distortion.sav")'), $
                         subdir=[date, 'level1'], $
                         root=process_basedir)
print, save_filename, format='(%"saving results to %s...")'
save, im, im1, im2, $
      old_dist_im1, old_dist_im2, $
      new_dist_im1, new_dist_im2, filename=save_filename

; display results
comp_process_distortion_display, im1, im2, title='Uncorrected', range=range
comp_process_distortion_display, old_dist_im1, old_dist_im2, title='Old distortion', range=range
comp_process_distortion_display, new_dist_im1, new_dist_im2, title='New distortion', range=range

end
