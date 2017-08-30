1.3.0
  transmission of opal based on date
  not masking post
  better center finding (find center of each image) and masking
  remove .npts from L2 filenames (dynamics, polarization, daily images, movies)
  polarimetric correction/proper orientation of Q/U
  changed method for finding files to produce mean/median files from
  new distortion correction
  radial azimuth in quick_invert files
  added FITS keywords (DOI, BRANCH, POLFRAME, BACKGRND, DCEN{X,Y}{UP,LOW})
  perform an L1 file sanity check and send notification for issues like high temps
  distribute good_*.txt files
  better notification emails ("Sent from")
  better log messages (formatting and changing normal occurrences to lower priority)
  log epoch values used for run
  verfify processing of past days
