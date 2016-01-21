; docformat = 'rst'

;+
; Average, demodulate, and calibrate an input array of data and variances
; using the calibration information information in an input calibration
; structure.
;
; :Params:
;   data : in, required, type="fltarr(nx, ny, nstates)"
;     Data images, one for each observed polarimeter state
;   vars : in, required, type="fltarr(nx, ny, nstates)"
;     Variances corresponding to data
;   labels : in, required, type=strarr(nstates)
;     Strings labeling each image in the data. must have same name format as
;     cal_struct.upols
;   cal_struct : in, required, type=structure
;     Structure containing calibration information, as produced by
;     `comp_cal_example_script.pro`
;
; :Keywords:
;   stokeslabels : out, optional, type=strarr
;     4 element string containing the labels of the inverted Stokes images
;     ('I', 'Q', 'U', and/or 'V')
;
; Returns the [nx,ny,nstokes] image array of the inverted Stokes vector.
;
; Joseph Plowman
;-
function comp_calibrate_stokes, data, vars, labels, cal_struct, $
                                stokeslabels=stokeslabels
  compile_opt strictarr

  stokeslabels = ['I', 'Q', 'U', 'V']
  coef_images = comp_compute_coef_images(cal_struct, labels)
  return, comp_compute_calibrated_stokes(coef_images, data, vars)
end