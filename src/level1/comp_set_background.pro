; docformat = 'rst'

;+
; :Keywords:
;   n_uniq_polstates : in, required, type=long
;     the number of unique polarization states
;   n_uniq_wavelengths : in, required, type=long
;     the number of unique wavelengths
;-
pro comp_set_background, date_dir, primary_header, images_combine, $
                         n_uniq_polstates=np, n_uniq_wavelengths=nw
  compile_opt strictarr
  @comp_constants_common

  ; set BACKGRND for image
  comp_make_mask, date_dir, primary_header, mask
  center_wave_index = nw / 2

  ; grab correct part of images_combine as background
  temp_background = dblarr(nx, ny)
  for i = 0L, np - 1L do begin
    temp_background += images_combine[*, *, np * nw + i * nw + center_wave_index]
  endfor
  temp_background /= np

  background = median(temp_background[where(mask eq 1.0)])
  sxaddpar, primary_header, 'BACKGRND', background, $
            ' Median of masked line center background', format='(F10.3)', $
            after='TIME_HST'


end
