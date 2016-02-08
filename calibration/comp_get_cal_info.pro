; docformat = 'rst'

;+
; Function to scan a set of files and return a structure listing the
; calibration files contained and their details.
;
; :Returns:
;   structure detailing calibration files matching whichwave, which has the
;   following form::
;
;     files:
;       String array of filenames.
;     waves:
;       String array of wavelengths (should match whichwave).
;     cpols:
;       Array of calibration polarizer states (should all be 1).
;     crets:
;       Array of calibration retarder states (1 = in, 0 = out).
;     cangs:
;       Array of calibration polarizer angles (note that there is a -1.4
;       degree offset and a value of 0 means it's a flat with the polarizer
;       removed).
;     mpols:
;       String array, dimensions (nfiles, npchans), listing all the
;       polarization analyzer states contained in the file. If the file has
;       fewer than npchans, the trailing entries will be blank strings ('').
;     cvers:
;       Array of instrument cover flags. Should all be 1 (i.e., no darks).
;     ctags:
;       String array of tags uniquely specifying calibration optics
;       configurations. Consists of cpol, cret, and cang (rounded to nearest
;       degree), space separated.
;
; :Params:
;   files : in, required, type=strarr
;     string array of the filenames to check
;
; :Keywords:
;   npchans : in, optional, type=integer, default=6
;     number of polarization channels expected
;   whichwave : in, optional, type=integer, default=1075
;     which line to check for - median wavelength rounded to nearest nanometer
;
; :Author:
;   Joseph Plowman
;-
function comp_get_cal_info, files, npchans=npchans, whichwave=whichwave
  compile_opt strictarr

  if (n_elements(npchans) eq 0) then npchans = 6
  if (n_elements(wave) eq 0) then whichwave = 1075

  nfiles = n_elements(files)
  filesout = strarr(nfiles)
  waves = lonarr(nfiles) ; The central wavelength in the file, rounded to nearest nanometer
  cpols = lonarr(nfiles) ; Whether or not the polarizer flag is set
  crets = lonarr(nfiles) ; Whether or not the retarder was in
  cangs = fltarr(nfiles) ; Calibration polarizer angles
  cvers = lonarr(nfiles) ; Is this a dark?
  ctags = strarr(nfiles) ; String which uniquely identifies cal setting
  mpols = strarr(nfiles, npchans) ; Measured polarizations in each file
  count = 0
  for i = 0, nfiles - 1 do begin
    fits_open, files[i], fcb
    inventory, fcb, beam, group, wave, pols, type, expose, cover, cal_pol, $
               cal_ret, cal_ang=cal_ang
    fits_close, fcb
    if (cal_pol eq 0) then cal_ang = 0.0
    if (cal_pol and round(median(wave)) eq whichwave and cover) then begin
      waves[count] =round(median(wave))
      cpols[count] = cal_pol
      crets[count] = cal_ret
      cangs[count] = cal_ang
      cvers[count] = cover
      ctags[count] = strjoin(strtrim(string(round([cal_pol, cal_ret, cal_ang])), 2), ' ')
      upols=pols[uniq(pols, sort(pols))]
      mpols[count, 0:n_elements(upols) - 1] = upols
      filesout[count] = files[i]
      count++
    endif
  endfor

  return, {files:filesout[0:count - 1], $
           waves:waves[0:count - 1], $
           cpols:cpols[0:count - 1], $
           crets:crets[0:count - 1], $
           cangs:cangs[0:count - 1], $
           mpols:mpols[0:count - 1, *], $
           cvers:cvers[0:count - 1], $
           ctags:ctags[0:count - 1]}
end
