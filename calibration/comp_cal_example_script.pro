.compile comp_calibration_subroutines.pro
.compile get_polarimeter_coefficients.pro
.compile compute_comp_calibration.pro

beam=-1
wave=1075
cal_directory = '/hao/solar4/plowman/CoMP/raw/20150729/'
plot_dir = '/hao/solar4/plowman/CoMP/calibration_plots2/'
coef_plot_dir = '/hao/solar4/plowman/CoMP/calibration_coef_plots2/'
if(n_elements(reload) eq 0 or keyword_set(reload)) then init_powfunc_comblk, cal_directory, wave, beam
reload=0

common comp_cal_comblk, xybasis, xyb_upper, xyb_lower, dataupper, datalower, varsupper, varslower, $
		xmat, ymat, cpols, pangs, crets, upols, datapols, datacals, cal_data, uppercoefs, $
		lowercoefs, uppermask, lowermask, data, vars, mask, nstokes, ucals, calvars, calvar_solve

calvar_labels = ['I in','Q in','U in','V in','Pol trans','P ang err','Ret tran','Retardance','Ret ang']

; This vector holds the calibration optics variables:
calvars = dblarr(9)
calvars[0:3] = [1.,0.,0.,0.] ; The input Stokes vector.
calvars[4] = 0.45 ; Calibration polarizer transmission.
calvars[5] = 0.0 ; Systematic offset error in the polarizer angle (in degrees).
calvars[6] = 0.99 ; Calibration retarder transmission.
calvars[7] = 94.438 ; Calibration retarder retardance (in degrees).
calvars[8] = 0.0 ; Calibration retarder angle (in degrees).

; Scales for initial guesses:
scales = dblarr(9)
scales[0:3] = [0.05,0.05,0.05,0.05] ; The input Stokes vector.
scales[4] = 0.05 ; Calibration polarizer transmission.
scales[5] = 5.0 ; Systematic offset error in the polarizer angle (in degrees).
scales[6] = 0.01 ; Calibration retarder transmission.
scales[7] = 5.0 ; Calibration retarder retardance (in degrees).
scales[8] = 5.0 ; Calibration retarder angle (in degrees).

; Flags for which calibration variables the amoeba should search for:
solve_flags = intarr(9)
solve_flags[0:3] = [0,0,0,0] ; The input Stokes vector.
solve_flags[4] = 0 ; Calibration polarizer transmission.
solve_flags[5] = 0 ; Systematic offset error in the polarizer angle.
solve_flags[6] = 0 ; Calibration retarder transmission.
solve_flags[7] = 1 ; Calibration retarder retardance.
solve_flags[8] = 1 ; Calibration retarder angle.

calvar_solve = where(solve_flags)
guess = calvars[calvar_solve]
scale = scales[calvar_solve]
res=amoeba(1.0e-5,function_name='comp_cal_powfunc',p0=guess,scale=scale)

print, 'Final chi squared = ', comp_cal_powfunc(res,diag_plot_dir = plot_dir)
for i=0,8 do print,calvar_labels[i],'= ',calvars[i]

; This structure holds the essential calibration information:
cal_struct = {xybasis:xybasis, xmat:xmat, ymat:ymat, cpols:cpols, pangs:pangs, crets:crets, upols:upols, $
		datapols:datapols, datacals:datacals, uppercoefs:uppercoefs, uppermask:uppermask, mask:mask, $
		ucals:ucals, calvars:calvars, calvar_solve:calvar_solve, calvar_labels:calvar_labels}
save, cal_struct, filename = cal_directory+'calibration_structure.sav'

make_coef_plots, coef_plot_dir