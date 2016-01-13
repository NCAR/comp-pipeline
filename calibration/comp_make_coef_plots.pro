; docformat = 'rst'

;+
; Makes plots of the crosstalk coefficients product by the calibration code, using information stored in the
; common block. Produces one plot file for each polarization analyzer state, containing plots of the
; to each component of the stokes vector (I, Q, U, and V). Takes as input the name of the directory
; to which the plots should be written.
;
; :Author:
;   Joseph Plowman
;-
pro comp_make_coef_plots, plot_directory
  compile_opt strictarr

  common comp_cal_comblk, xybasis, xyb_upper, xyb_lower, dataupper, datalower, varsupper, varslower, $
      xmat, ymat, cpols, pangs, crets, upols, datapols, datacals, cal_data, uppercoefs, $
      lowercoefs, uppermask, lowermask, data, vars, mask, nstokes, ucals, calvars, calvar_solve

  ; Numbers of unique polarizations, x and y pixels, and spatial basis elements:
  nupols = n_elements(upols)
  nx = n_elements(xybasis[*, 0, 0])
  ny = n_elements(xybasis[0, *, 0])
  n_basis = n_elements(xybasis[0, 0, *])

  ; The usual names for the Stokes vector components
  stokeslabels = ['I', 'Q', 'U', 'V']

  ; Array to hold the images
  coef_image = dblarr(nx,ny)

  ; Set up the plotting device:
  set_plot, 'ps'
  device, color=1, bits_per_pixel=8, decomposed=0, /encapsulated, $
          xsize=10, ysize=10, /inches
  !p.multi = [0, 2, 2]

  ; Color table which goes from purple to black to green:
  r = [0, reverse(dindgen(127)), dblarr(127), 255]
  b = [0, dblarr(127), dindgen(127), 255]
  tvlct, r, b, r

  if (~file_test(plot_directory)) then file_mkdir, plot_directory
  ; Loop over unique polarizations (i.e., analyzer states; nominally I+Q, I+U, etc):
  for i=0,nupols-1 do begin
    ; Set the plot file name.
    device, filename=filepath('cal_coef_'+upols[i]+'.eps', root=plot_directory)
    for j=0,nstokes-1 do begin ; Loop over Stokes vector components.
      coef_image *= 0.0        ; Zero out the image to start.
      ; Loop over the basis elements, adding up the components of the image:
      for k=0,n_basis-1 do begin
        upperimage = mask*(uppercoefs[i,j*n_basis+k]*uppermask)*xybasis[*,*,k]
        lowerimage = mask*(lowercoefs[i,j*n_basis+k]*lowermask)*xybasis[*,*,k]
        coef_image += upperimage+lowerimage
      endfor
      ; Make the plot, with intensity range centered on zero:
      min_str=strtrim(string(min(coef_image[where(mask)])),2)
      max_str=strtrim(string(max(coef_image[where(mask)])),2)
      pmax = max(abs(coef_image[where(mask)]))
      plot_image,coef_image,title=stokeslabels[j]+', min='+min_str+', max=' $
                 +max_str,top=254,bottom=1,min=-pmax,max=pmax
    endfor
    device,/close
  endfor
end
