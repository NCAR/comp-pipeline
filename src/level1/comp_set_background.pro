; docformat = 'rst'

;+
; Set the "BACKGRND" FITS keyword in the primary header.
;
; :Params:
;   date_dir : in, required, type=string
;     the date directory for this data
;   primary_header : in, out, required, type=starr
;     primary header
;   images_combine : in, required, type="fltarr(620, 620, (2*np*nw))"
;     the combined images, one foreground, one background for each wavelength
;     and stokes component. The last index is sorted first by wavelength
;     (successive wavelengths are adjacent), then by polarization
;   headers_combine : in, required, type="strarr(ntags2, (2*np*nw))"
;     headers corresponding to `images_combine`; the 'BEAM' tag, if present,
;     will be removed from the header; `ntags2` will therefore generally be
;     equal to `ntags - 1`
;
; :Author:
;   MLSO Software Team
;-
pro comp_set_background, date_dir, primary_header, images_combine, headers_combine
  compile_opt strictarr
  @comp_constants_common

  ; set BACKGRND for image
  mask = comp_l1_mask(primary_header)

  background = comp_get_component(images_combine, headers_combine, 'BKGI', 0, $
                                  /noskip, /average_wavelengths, n_wavelengths=3)
  backgrnd = median(background[where(mask eq 1.0)])

  sxaddpar, primary_header, 'LCBKG', backgrnd, $
            ' Median of masked line center intensity background', format='(F10.3)', $
            after='TIME_HST'
end
