1.0.0 [Jun 1, 2016]
1.1.0 [Jun 10, 2016]
1.2.4 [Sep 29, 2016]
1.3.0 [Nov 20, 2017]
  transmission of opal based on date
  better center finding (find center of each image) and masking
  polarimetric correction/proper orientation of Q/U
  not masking post
  display parameters adjusted
  remove .npts from L2 filenames (dynamics, polarization, daily images, movies)
  changed method for finding files to produce mean/median files from
  new distortion correction
  radial azimuth in quick_invert files
  added FITS keywords (DOI, BRANCH, POLFRAME, BACKGRND, DCEN{X,Y}{UP,LOW})
  perform an L1 file sanity check and send notification for issues like high temps
  better notification emails ("Sent from")
  distribute good_*.txt files
  better log messages (formatting and changing normal occurrences to lower priority)
  log epoch values used for run
  verify processing of past days
1.3.1 [Nov 27, 2017]
  velocity calculation fixes
  fixed L2 distribution bug
  revised GBU
  plot centering diagnostics by wave type
1.3.2 [Nov 27, 2017]
  fix for averaging parameters
1.3.3 [Nov 27, 2017]
  additional fix for averaging parameters
1.3.4 [Nov 27, 2017]
  fix for MEDIAN error in doppler correction
1.3.5 [Nov 27, 2017]
  fix typo in log message variable in GBU
1.3.6 [Nov 28, 2017]
  fix for plotting centering information
1.3.7 [Nov 29, 2017]
  typo in plotting centering information
  remove existing crosstalk and centering logs when starting
1.3.8 [Nov 29, 2017]
  fixes in clearing existing engineering logs when starting
1.3.9 [Nov 30, 2017]
  cuts down on number of warnings in logs about temperatures
  does not delete engineering logs if raw dir is locked
1.3.10 [Feb 1, 2018]
  limits search for post
  add "comp" to all output filenames and more consistency in filenames
  fixed bugs in GBU
  add GBU problems to L1 check notification
  add warnings for too great of a post angle difference between beams
  removing useless warnings from logs
  set notification email from: field
  condensed notification emails to a single email
  1074 background GIFs
  fixed offsensor warnings
  added reprocess step
1.3.11 [Feb 1, 2018]
  removing threshold for wings on gaussian fit
1.3.12 [Feb 2, 2018]
  fixes for logging and reprocessing
1.3.13 [Feb 2, 2018]
  another fix for rotate log interaction with reprocessing
1.3.14 [Feb 2, 2018]
  put back restrictions on wings
1.3.15 [Feb 2, 2018]
  typo in wing restriction fix
1.3.16 [Feb 28, 2018]
  fix bug in distributing quick invert files
  fix distributing mean/median/sigma average files
  use coronal beam for 1083 centering
  bug fix for when no files to process in a wavelength
1.3.17 [Mar 5, 2018]
  more fixes for when no files to process in a wavelength
1.3.18 [Mar 5, 2018]
  bug fix
2.0.0 [Mar 18, 2021]
  quality check
  update database
  level 1 summary text file
  produce GIFs and mp4 of science images not passing quality
  produce GBU plot for the day
  fix for bad colors/colorbar in GIF
  better ephemeris calculation
  new averaging scheme
  not combining beams for 1083
  add RAWEXT FITS keyword
  combine matching extensions early in the pipeline
  add epoch value to specify a routine to repair raw data
  routing file for raw and processed base directories
2.0.1 [Mar 24, 2021]
  fix for finding files to average with only a single candidate file
  not marking a day as processed if crashed
  adding sub-commands to comp script (list, report)
2.0.2 [May 26, 2021]
  updating GBU constants per epoch
  handle performing GBU after level 1 processing
2.0.3 [May 27, 2021]
  fix for SQL syntax error
2.0.4 [June 7, 2021]
  more fixes for leaking FITS fcb LUNs
2.0.5 [June 12, 2021]
  fix for centering plots with only a single file
  fix for incorrect NTUNES in mean/median average files for waves files
2.0.6 [June 23, 2021]
  adjustments to level 2 masking to make quick invert and analytical versions the same
  change display min/max for Q/U
  changing GBU threshold for sigma to 1.25
2.0.7 [Nov 9, 2021]
  fix for only a single image to average in COMP_AVERAGE
  handle malformed cal files
  fix 1083 intensity FITS files
2.0.8 [Nov 15, 2021]
  handle no GBU file for a wave region
2.0.9 [Nov 16, 2021]
  another fix for only a simple image to average in COMP_AVERAGE
  fix for crash in error handling in COMP_CAL_INSERT
2.0.10 [Jan 10, 2022]
  adjusting GBU max threshold
2.0.11 [Feb 18, 2022]
  adding r110_intensity_mean and r110_background_mean to comp_sci table
2.0.12 [Jun 3, 2022]
  intensity in polarization level 2 file is now sum of middle 3 wavelengths / 2
  added ERESTWVL and WRESTWVL FITS keywords for velocity in dynamics file
2.0.13 [Jun 15, 2022]
  change occulter overmasking from 1 pixel to 2 pixels, field stop from 2
    pixels to 3
  overmask field stop an additional 2 pixels for GBU calculations
2.0.14 [Jun 15, 2022]
  fix for bad 2.0.13 release
2.0.15
