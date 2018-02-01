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
  verfify processing of past days
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
1.3.11
