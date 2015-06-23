; docformat = 'rst'

;+
; Fits CoMP crosstalk from I, Q, and U into V using a simple linear least
; squares fit to a model of the form::
;
;   Vx = I * (aI + bI * x + cI * y + dI * x * y)
;          + Q *(aQ + bQ * x + cQ * y + dQ * x * y)
;          + U(...),
;
; where x and y are the pixel coordinates in the image (the crosstalk is thus
; spatially varying), and aI, aQ, aU, bI, etc are the (12 in total) parameters
; of the fit. To do the fit, we assume that the input Stokes V image is
; dominated by crosstalk. Typically we use wavelength averaged offband CoMP
; images (which should have no actual Stokes V signal) for this reason.
;
; :Params:
;   date_dir : in, required, type=string
;     name of directory containing the files for the date of the input image,
;     used for making a mask to eliminate areas of the image obscured by the
;     occulter and field stop
;   Iin : in, required, type="fltarr(nx, ny)"
;     The input Stokes I image.
;   Qin : in, required, type="fltarr(nx, ny)"
;     The input Stokes Q image.
;   Uin : in, required, type="fltarr(nx, ny)"
;     The input Stokes U image.
;   Vin : in, required, type="fltarr(nx, ny)"
;     an input Stokes V image, which is assumed to be dominated by crosstalk
;   Headers : in, required, type=strarr
;     a set of headers, which are used to make a mask to eliminate areas of the
;     image obscured by the occulter or field stop
;   IVxtalk : out, required, type="fltarr(nx, ny)"
;     the Stokes I to V crosstalk coefficient array
;   QVxtalk : out, required, type="fltarr(nx, ny)"
;     the Stokes Q to V crosstalk coefficient array
;   UVxtalk : out, required, type="fltarr(nx, ny)"
;     the Stokes U to V crosstalk coefficient array
;   xtparms : out, optional, type=fltarr(12)
;     the crosstalk parameters, organized [aI, aQ, aU, bI, ...]
;
; :Author:
;   Joseph Plowman
;-
pro comp_find_vxtalk, date_dir, Iin, Qin, Uin, Vin, headers, $
                      IVxtalk, QVxtalk, UVxtalk, xtparms
  compile_opt strictarr

  ; Get mask to ensure we don't try to fit areas obscured by occulter or field stop:
  mask0 = comp_raw_mask(date_dir, headers, $
                        upper_left_mask=mask1, lower_right_mask=mask2)
  xtmask = mask1 or mask2
  nxtalks = 3   ; one each for I, Q, and U

  ; wavelength average (if more than one wavelength) and apply mask
  if (n_elements(Iin[0, 0, *]) gt 1) then begin  
    vcross = mean(Vin, dimension=3) * xtmask
    quvparms = [[[mean(Iin, dimension=3)]], $
                [[mean(Qin, dimension=3)]], $
                [[mean(Uin,dimension=3)]]]
  endif else begin
    vcross = Vin * xtmask
    quvparms = [[[Iin]], [[Qin]], [[Uin]]]
  endelse
  for i = 0L, nxtalks - 1L do quvparms[*, *, i] *= xtmask

  ; estimate of shot noise
  shotfac = 0.1
  vcerrs = shotfac * sqrt(Iin * shotfac) * xtmask

  nx = n_elements(vcross[*, 0])
  ny = n_elements(vcross[0, *])

  ; have 4 parameters for each crosstalk: constant, linear in x, linear in y,
  ; and bilinear
  nperxtalk = 4
  nfitparms = nxtalks * nperxtalk

  ; A bunch of linear algebra follows, obtaining by minimizing
  ;
  ; chi^2 = (V - Vmodel)^2 / sigma^2, where Vmodel is of the form
  ; Vx = I * (aI + bI * x + cI * y + dI * x * y)
  ;        + Q * (aQ + bQ * x + cQ * y + dQ * x * y)
  ;        + U(...)
  ;
  ; This is a linear model in the parameters (a,b,c,d), so chi^2 is quadratic.
  ; The best fit will occur where the vector derivative of chi^2 is zero; the
  ; math below is found by solving for that zero.
  smat = dblarr(nx, ny, nfitparms)
  xmat = (dindgen(nx)) # transpose(1.0 + dblarr(ny)) - 0.5 * (nx - 1.0)
  ymat = (1.0D + dblarr(nx)) # transpose(dindgen(ny)) - 0.5 * (ny - 1.0)

  for i = 0L, nperxtalk - 1L do begin
    smat[*, *, i * nxtalks:(i + 1) * nxtalks - 1] = quvparms
  endfor
  for i = 0L, nxtalks - 1L do smat[*, *, nxtalks + i] *= xmat
  for i = 0L, nxtalks - 1L do smat[*, *, 2 * nxtalks + i] *= ymat
  for i = 0L, nxtalks - 1L do smat[*, *, 3 * nxtalks + i] *= ymat * xmat

  amat = dblarr(nfitparms, nfitparms)
  bvec = dblarr(nfitparms)

  for k = 0L, nfitparms - 1L do begin
    for l = 0L, nfitparms - 1L do begin
      amat[k, l] = total(smat[*, *, k] * smat[*, *, l] / vcerrs^2.0, /nan)
    endfor
  endfor

  for l = 0L, nfitparms - 1L do begin
    bvec[l] = total(smat[*, *, l] * vcross / vcerrs^2.0, /nan)
  endfor

  ainv = invert(amat, /double)
  xtparms = ainv#bvec

  ; unpack the matrix of solved crosstalk parameters and assign
  ; it to the individual crosstalk arrays:
  xtmat = dblarr(nx,ny,nxtalks)
  for i = 0L, nxtalks - 1L do begin
    xtmat[*, *, i] = xtparms[i] + xmat * xtparms[i + nxtalks] $
                       + ymat * xtparms[i + 2 * nxtalks] $
                       + xmat * ymat * xtparms[i + 3 * nxtalks]
  endfor

  IVxtalk = xtmat[*, *, 0]
  QVxtalk = xtmat[*, *, 1]
  UVxtalk = xtmat[*, *, 2]
end