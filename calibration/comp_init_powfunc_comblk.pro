; docformat = 'rst'

;+
; Initialize the common block used by comp_cal_powfunc, which is called by
; Amoeba or Powell to compute the crosstalk coefficients and corresponding chi
; squared for a given input stokes vector and calibration optic configuration.
; This involves sifting though the cal files, organizing them according to the
; instrument polarization analyzer state (i.e., nominally I+Q, I-Q, I+U, etc),
; and assigning them into the correct spot in the common block (Amoeba and
; Powell force the use of a common block for data and constants).
;
; The common block has the following components::
;
;   xybasis:
;     The set of functions which define the spatial variation of the crosstalk
;     (see `COMP_CAL_XYBASIS`), dimensions are `[nx, ny, nbasis]`, where
;     `nbasis` is 4 by default.
;    xmat:
;      Array of `x` pixel coordinates corresponding to `xybasis`.
;    ymat:
;      Array of `y` pixel coordinates corresponding to `xybasis`.
;    data:
;      The data. There is one image for each unique combination of polarization
;      analyzer configuration (I+Q, I+U, etc) and cal optics configuration
;      (polarizer present, retarder present, polarizer angle). Multiple images
;      which have the same analyzer and cal optics configuration are averaged.
;      Dimensions are `[nx, ny, n_upols * n_ucals]`, where `n_upols` and
;      `n_ucals` are the number of unique analyzer and calibration optics
;      configurations, respectively.
;    vars:
;      The variances corresponding to the data, estimated by assuming shot
;      noise dominates. There is one image for each unique combination of
;      polarization analyzer configuration (I+Q, I+U, etc) and cal optics
;      configuration (polarizer present, retarder present, polarizer angle).
;    mask:
;      Mask image `[nx, ny]` indicating which pixels are good. This is
;      determined from the flats and by comparison to a median smoothing.
;    uppermask:
;      Mask of pixels above the diagonal (`ymat gt xmat`).
;    lowermask:
;      Mask of pixels below the diagonal (`xmat gt ymat`).
;    xyb_upper:
;      Trimmed down version of `xybasis` containing only those values for the
;      unmasked pixels above the diagonal. This and all other 'upper' arrays
;      replace the spatial indices (two of them, one for x and one for y) with
;      a single index based on where(mask*uppermask) - roughly,
;
;        xyb_upper[*, i] = xybasis_i[where(mask * uppermask)],
;
;      with `xybasis_i = xybasis[*,*,i]`. We work only with these arrays for
;      the most time intensive part of the calculation, speeding up execution.
;    xyb_lower:
;      Trimmed down version of xybasis containing only those values for the
;      unmasked pixels below the diagonal. This and all other 'lower' arrays
;      replace the spatial indices (two of them, one for x and one for y) with
;      a single index based on `where(mask * lowermask)`.
;    dataupper:
;      Trimmed down version of data containing only those values for the
;      unmasked pixels above the diagonal.
;    dataupper:
;      Trimmed down version of data containing only those values for the
;      unmasked pixels below the diagonal.
;    varsupper:
;      Trimmed down version of variances containing only those values for the
;      unmasked pixels above the diagonal.
;    varslower:
;      Trimmed down version of variances containing only those values for the
;      unmasked pixels below the diagonal.
;    cpols:
;      Array giving the cal polarizer state (1 = in, 0 = out) for each image in
;      the data.
;    pangs:
;      Array giving the cal polarizer angle for each image in the data.
;    crets:
;      Array giving the cal retarder state (1 = in, 0 = out) for each image in
;      the data.
;    upols:
;      String array listing each unique polarization analyzer configuration
;      (i.e., I+Q, I+U, etc) present.
;    ucals:
;      String array listing each unique calibration optics configuration
;      (i.e., I+Q, I+U, etc) present.
;    datapols:
;      String array listing the polarization analyzer configuration for each
;      image in data.
;    datacals:
;      String array specifying the calibration optics configuration for each
;      image in data. the labels consist of a polarizer state flag (1 = in,
;      0 = out), retarder state flag (1 = in, 0 = out), and the polarizer angle
;      listed in the file, rounded to the nearest degree.
;    cal_data:
;      When comp_cal_powfunc is run, it will store its fit to the data in this
;      array. Has the same dimensions as data.
;    uppercoefs:
;      When comp_cal_powfunc is run, it will store the coefficients for the fit
;      above the diagonal in this array.
;    lowercoefs:
;      When comp_cal_powfunc is run, it will store the coefficients for the fit
;      below the diagonal in this array.
;    nstokes:
;      The number of components in a stokes vector (will almost always be 4 -
;      this is only placed here to clean up the code a bit).
;
; :Params:
;   cal_directory : in, required, type=string
;     the directory containing the cal files
;   wave :  in, required, type=string
;     which line to use (median wavelength rounded to nearest nm)
;   beam : in, required, type=integer
;     which beam splitter setting to use (+1 or -1)
;   date_dir : in, required, type=string
;   exact_wave : in, required, type=float
;     exact wavelength to use for run
;   location : in, required, type=lonarr(2)
;     location to center 5x5 mask for data
;
; :Author:
;   Joseph Plowman
;-
pro comp_init_powfunc_comblk, cal_directory, wave, beam, date_dir, exact_wave, location
  compile_opt strictarr
  common comp_cal_comblk, xybasis, xyb_upper, xyb_lower, dataupper, datalower, $
                          varsupper, varslower, xmat, ymat, cpols, pangs, $
                          crets, upols, datapols, datacals, cal_data, $
                          uppercoefs, lowercoefs, uppermask, lowermask, $
                          data, vars, mask, nstokes, ucals, calvars, $
                          calvar_solve

  s = replicate(1, 5, 5)   ; array for 5x5 erode function to pad the mask
  nstokes = 4   ; Stokes vector has 4 components
  ; conversion factor from disk intensity to photons (for 250ms exposures)
  photfac = double(1.0 / sqrt(875.0))

  ; standard CoMP images are 1k by 1k, before beam combining
  if (n_elements(nx) eq 0) then nx = 1024
  if (n_elements(ny) eq 0) then ny = 1024

  ; find the calibration files in the cal directory
  files = file_search(cal_directory + '*.FTS')
  cal_info = comp_get_cal_info(files)

  ; the files which have the correct line ('wave'), are not darks ('cvers') and
  ; are labeled as cal data
  calfiles = where(cal_info.waves eq wave and cal_info.cvers and cal_info.cpols)
  nfiles = n_elements(calfiles)   ; total number of cal files.

  ; the number polarization analyzer states in each file
  n_mpols = total(cal_info.mpols ne '', 2)

  ; find the unique measured polarizations
  upols = cal_info.mpols[uniq(cal_info.mpols, sort(cal_info.mpols))]
  upols = upols[where(upols ne '')]
  n_upols = n_elements(upols)

  ; find the unique cal optics configurations
  ucals = cal_info.ctags[uniq(cal_info.ctags, sort(cal_info.ctags))]
  n_ucals = n_elements(ucals)

  ; total number of images needed for all unique combinations of polarization
  ; analyzer and calibration optics configurations
  ndata = total(n_upols * n_ucals)

  ; arrays which will label the polarization analyzer and calibration optics
  ; states for each image in the data array
  datapols = strarr(ndata)
  datacals = strarr(ndata)
  for i = 0, n_upols - 1 do begin
    for j = 0, n_ucals - 1 do begin
      datapols[i * n_ucals + j] = upols[i]
      datacals[i * n_ucals + j] = ucals[j]
    endfor
  endfor

  ; compute the spatial basis images
  xybasis = comp_cal_xybasis(nx=nx, ny=ny, xmat=xmat, ymat=ymat)
  uppermask = ymat gt xmat   ; mask for pixels above the diagonal...
  lowermask = xmat gt ymat   ; mask for pixels below the diagonal...
  nbasis = n_elements(xybasis[0, 0, *])

  ; array containing data for each unique configuration
  data = fltarr(nx, ny, ndata)
  ; variances corresponding to data (assumes only shot noise)
  vars = fltarr(nx, ny, ndata)
;  navgs = fltarr(ndata) ; number of CoMP exposures went into each image in data
  cpols = dblarr(ndata) ; polarization analyzer flags for each image in data
  crets = dblarr(ndata) ; calibration retarder flags for each image in data
  pangs = dblarr(ndata) ; calibration polarizer angle for each image in data
  datacount = lonarr(ndata) ; counts how many images were averaged for each element of data
  mask = uppermask or lowermask ; initialize the mask

  ; how many images are used in max_images of raw_data
  n_images = lonarr(ndata)

  for i = 0, nfiles - 1 do begin
    ii = calfiles[i]   ; select the next cal file
    mg_log, 'checking %s...', cal_info.files[ii], name='comp', /debug
    comp_read_data, cal_info.files[ii], images, headers, header0
    cal = cal_info.ctags[ii] ; cal optics state for this file (assumes only one per file)

    ; loop over the measured polarizations in the file
    for j = 0, n_mpols[ii] - 1 do begin
      pol = cal_info.mpols[ii, j]   ; the current measured polarization

      idata = where(datapols eq pol and datacals eq cal)

      comp_inventory_header, headers, beams, groups, waves, polstates, type, $
                             expose, cover, cal_pol, cal_ret

      beams[0] = 2  ; never match first image in a file
      !null = where(waves eq exact_wave and polstates eq pol and beams eq beam, n_matching_waves)
      n_images[idata] += n_matching_waves
      print, file_basename(cal_info.files[ii]), cal, pol, n_matching_waves, $
             format='(%"%s: cal: %s, pol: %s, n_matching_waves: %d")'
    endfor
  endfor

  ; all used images before median
  max_images = max(n_images)
  raw_data = fltarr(nx, ny, max_images, ndata)

  n_found_images = lonarr(ndata)

  for i = 0, nfiles - 1 do begin
    ii = calfiles[i]   ; select the next cal file
    mg_log, 'reading %s...', cal_info.files[ii], name='comp', /debug
    comp_read_data, cal_info.files[ii], images, headers, header0
    comp_apply_flats_darks, images, headers, date_dir, flat_header=flat_header
    cal = cal_info.ctags[ii] ; cal optics state for this file (assumes only one per file)

    ; loop over the measured polarizations in the file
    for j = 0, n_mpols[ii] - 1 do begin
      pol = cal_info.mpols[ii, j]   ; the current measured polarization
      ; where this polarization and calibration combo lands in the data array
      ; (which index)
      idata = where(datapols eq pol and datacals eq cal)
      idata = idata[0]

      comp_inventory_header, headers, beams, groups, waves, polstates, type, $
                             expose, cover, cal_pol, cal_ret

      beams[0] = 2  ; never match first image in a file
      ind = where(waves eq exact_wave and polstates eq pol and beams eq beam, n_matching_waves)

      for w = 0L, n_matching_waves - 1L do begin
        raw_data[*, *, n_found_images[idata], idata] = images[*, *, ind[w]]
        n_found_images[idata]++
      endfor

      ; get the current file's data for this polarization analyzer state
;      datai = comp_get_component(images, headers, pol, beam, exact_wave, $
;                                 headersout=headersout)
      ; add it do the data and variance arrays (weighting by the number of
      ; exposures)
;      data[*, *, idata] += datai * sxpar(headersout, 'NAVERAGE')
;      vars[*, *, idata] += photfac * abs(datai) * sxpar(headersout, 'NAVERAGE')
      ; increment the total number of exposures (so we can
      ; renormalize later)
;      navgs[idata] += sxpar(headersout, 'NAVERAGE')
      datacount[idata] += 1

      ; make sure we've recorded book keeping data for this index
      cpols[idata] = cal_info.cpols[ii]
      crets[idata] = cal_info.crets[ii]
      pangs[idata] = cal_info.cangs[ii]

      ; compute the mask for this file
      comp_make_mask_1024, date_dir, flat_header, mask0, o_offset=4.0, f_offset=-3.0
      ; pad the file's mask and combine it with the main mask
      ;mask *= erode(mask0, s)
      mask *= mask0
      ; remove outlier pixels via median filtering
      ;mask *= abs(datai / median(datai, 3) - 1.0) lt 0.25
    endfor
  endfor

  for d = 0L, ndata - 1L do begin
    data[*, *, d] = median(raw_data[*, *, 0:n_images[d] - 1, d], dimension=3)
    ;vars[*, *, d] = photfac * abs(data[*, *, d]) /  n_found_images[d]
    vars[*, *, d] = abs(data[*, *, d])
  endfor

  ; mask by the 5x5 block around the location
  loc_mask = bytarr(nx, ny)
  loc_mask[location[0] - 2:location[0] + 2, location[1] - 2: location[1] + 2] = 1B
  mask and= loc_mask

  ; if some calibration and polarizer analyzer combinations are absent from the
  ; data, remove them from the common block arrays
  wherepresent = where(datacount gt 0)
  data      = data[*, *, wherepresent]
  vars      = vars[*, *, wherepresent]
;  navgs     = navgs[wherepresent]
  cpols     = cpols[wherepresent]
  crets     = crets[wherepresent]
  pangs     = pangs[wherepresent]
  ndata     = total(datacount gt 0)
  datacount = datacount[wherepresent]

  ; initialize flattened upper and lower arrays, used to speed some parts of
  ; calculation
  whereupper = where(uppermask * mask, npix_upper)
  wherelower = where(lowermask * mask, npix_lower)

  if (npix_upper gt 0) then begin
    dataupper = dblarr(npix_upper, ndata)
    varsupper = dblarr(npix_upper, ndata)
    xyb_upper = dblarr(npix_upper, nbasis)
  endif else dataupper = !null

  if (npix_lower gt 0) then begin
    datalower = dblarr(npix_lower, ndata)
    varslower = dblarr(npix_lower, ndata)
    xyb_lower = dblarr(npix_lower, nbasis)
  endif else datalower = !null

  ; assign values to flattened upper and lower basis function arrays
  for i = 0, nbasis - 1 do begin
    xyb_i = reform(xybasis[*, *, i])
    if (n_elements(dataupper) gt 0) then xyb_upper[*, i] = xyb_i[whereupper]
    if (n_elements(datalower) gt 0) then xyb_lower[*, i] = xyb_i[wherelower]
  endfor

  for i = 0, ndata - 1 do begin
    ; renormalize the data by total exposure time
;    datai = reform(data[*, *, i] / navgs[i])  ;/datacount[i])
;    varsi = reform(data[*, *, i] / navgs[i])  ;/datacount[i])
;    data[*, *, i] = datai
;    vars[*, *, i] = varsi
    datai = data[*, *, i]
    varsi = vars[*, *, i]

    ; assign values to flattened upper and lower data and variance arrays
    if (n_elements(dataupper) gt 0) then begin
      dataupper[*, i] = datai[whereupper]
      varsupper[*, i] = varsi[whereupper]
    endif
    if (n_elements(datalower) gt 0) then begin
      datalower[*, i] = datai[wherelower]
      varslower[*, i] = varsi[wherelower]
    endif
  endfor
end
