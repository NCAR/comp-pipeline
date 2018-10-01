
wave_cal_10xx_2.txt format

col 1 - datedir
col 2 - local time (decimal hours)
col 3 - wavelength offset (nm) for solar spectrum from KPNO atlas
col 4 - telluric line strength index
col 5 - dirrerential telluric line wavelength offset (nm)

first row is on band
second row is off band

Steve will create routine to synthesize normalization factors to be applied to
flats starting with powfunc in calibrate_comp_wavelength_2 and inserting solar
and telluric spectrum lookups and comp transmission profile calculation
