comp_cal_example_script.pro is a script showing how the code is run
and producing some plots.

comp_calibration_subroutines.pro contains the various ancillary
subroutines used in the process of computing the calibration. Note
that get_crosstalk_xybasis has been renamed to comp_cal_xybasis and
moved to this file.

get_polarimeter_coefficients.pro contains only the core linear
inversion routines for the polarimeter coefficients.

compute_comp_calibration.pro contains the top-level routines called in
the process of computing the CoMP calibration (setting up the common
block, the function called by amoeba, plotting routines), as well as
any routine that uses the comp_cal_comblk common block.
