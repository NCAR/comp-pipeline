; docformat = 'rst'

;+
; Return an enhanced intensity image.
;
; :Uses:
;   sxpar, mpfitfun
;
; :Returns:
;   enhanced intensity image, `bytarr(620, 620)`
;
; :Params:
;   data : in, required, type="fltarr(620, 620)"
;     image
;   hdr : in, required, type=strarr
;     FITS header
;
; :Keywords:
;   status : out, optional, type=integer
;     set to a named variable to retrieve `MPFITFUN` status, <= 0 indicates
;     definite error
;   error_msg : out, optional, type=string
;     set to named variable to retrieve `MPFITFUN` error message, empty string
;     if no error
;
; :Author:
;   MLSO Software Team
;-
function comp_intensity_enhancement, data, hdr, status=status, error_msg=error_msg
  compile_opt strictarr

  if (sxpar(hdr, 'FRADIUS') ne 0 and sxpar(hdr, 'ORADIUS') ne 0) then begin
    r_inner = sxpar(hdr, 'ORADIUS')
    r_outer = sxpar(hdr, 'FRADIUS')
  endif else begin
    r_inner  = (sxpar(hdr, 'OCRAD1') + sxpar(hdr, 'OCRAD2')) / 2.
    r_outer  = ((sxpar(hdr, 'FCRAD1') + sxpar(hdr, 'FCRAD2')) / 2.) + 10.
  endelse
  xycenter = [sxpar(hdr, 'CRPIX1'), sxpar(hdr, 'CRPIX2')]
  
  ss = size(data)
  v_x = double(findgen(ss[1]))
  v_y = double(findgen(ss[2]))
  x = rebin(v_x,ss[1], ss[2]) - xycenter[0]
  y = rebin(transpose(v_y), ss[1], ss[2]) - xycenter[1]
  r = sqrt(x^2 + y^2)

  sr = sort(r)
  sx = r[sr]

  sim = data[sr]
  lminp = min(where(sx ge r_inner))
  maxp  = max(where(sx le r_outer))
  lx1 = sx[lminp:maxp]
  ly1 = sim[lminp:maxp]
  err = 1.0D
  start = [1.0D6, 50.0D]

  lfit = mpfitfun('comp_expfit', lx1, ly1, err, start, /nan, /quiet, $
                  errmsg=error_msg, status=status)
  limb = bytscl(data / comp_expfit(r, lfit), min=0, max=4)
  mlimb = unsharp_mask(limb, radius=3.0, amount=4.0, threshold=0.05)

  fitim = mlimb * (r ge r_inner - 1)

  return, fitim
end


; main-level example program

date = '20130103'
comp_initialize, date

process_basedir = '/hao/dawn/Data/CoMP/process'
filename = filepath('20130103.comp.1074.quick_invert.mean.synoptic.fts.gz', $
                    subdir=[date, 'level2'], $
                    root=process_basedir)
fits_open, filename, fcb
fits_read, fcb, data, primary_header, exten_no=0
fits_read, fcb, intensity, header, exten_no=1
fits_close, fcb

enhanced_intensity = comp_intensity_enhancement(intensity, header, $
                                                status=status, $
                                                error_msg=error_msg)

write_png, '20130103.comp.1074.enhanced_intensity.png', enhanced_intensity

end

