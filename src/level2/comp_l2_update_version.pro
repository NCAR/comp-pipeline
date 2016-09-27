; docformat = 'rst'

;+
; Update a level 1 header's VERSION/REVISION for a level 2 product.
;
; :Params:
;   header : in, out, required, type=strarr
;     FITS header
;-
pro comp_l2_update_version, header
  compile_opt strictarr

  ; get the L1 versions
  l1version = sxpar(header, 'VERSION', count=n_l1version)
  l1revision = sxpar(header, 'REVISION', count=n_l1revision)

  version = comp_find_code_version(revision=revision)
  sxaddpar, header, 'VERSION', version, ' Calibration software version'
  sxaddpar, header, 'REVISION', revision, ' Calibration software revision'

  if (n_l1version gt 0) then begin
    sxaddpar, header, 'L1VER', l1version, $
              ' Level 1 calibration software version', $
              after='REVISION'
  endif

  if (n_l1revision gt 0) then begin
    sxaddpar, header, 'L1REV', l1revision, $
              ' Level 1 procession software revision', $
              after='L1VER'
  endif
end
