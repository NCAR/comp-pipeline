; docformat = 'rst'

;+
; Update a level 1 header's VERSION/REVISION for a level 2 product.
;
; :Params:
;   header : in, out, required, type=strarr
;     FITS header
;
; :Author:
;   MLSO Software Team
;-
pro comp_l2_update_version, header, from_l2=from_l2
  compile_opt strictarr

  ; get the L1 versions

  ; L1VER/L1REV might already be present if creating an L2 file from another L2
  ; file

  l1version = sxpar(header, 'L1VER', count=n_l1version)
  if (n_l1version eq 0) then begin
    l1version = sxpar(header, 'VERSION', count=n_l1version)
  endif

  l1revision = sxpar(header, 'L1REV', count=n_l1revision)
  if (n_l1revision eq 0) then begin
    l1revision = sxpar(header, 'REVISION', count=n_l1revision)
  endif

  version = comp_find_code_version(revision=revision, branch=branch)
  sxaddpar, header, 'VERSION', version, $
            ' Calibration processing software version'
  sxaddpar, header, 'REVISION', revision, $
            ' Calibration processing software revision'
  sxaddpar, header, 'BRANCH', revision, $
            ' Calibration processing software branch', $
            after='REVISION'

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
