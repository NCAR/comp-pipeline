
fast_doppler_demod.pro
idl procedure to read in the raw data and apply bias, gain, distortion and other 
corrections and subtracts background image from coronal image to create coronal 
intensity images. The corrected intensity images are written to an output Level_1 
FITS file in the precess directory with one extension for each intensity image.
Output data file has name date.time.l1.fts

fast_doppler_compute_velocity.pro
Procedure to compute fast cadence doppler images and write them to a fits file. This
procedure will work on either one or two beam fast doppler data (two wavelengths only).
A velocity image is computed from either two images (one beam) or four images (two beam)
using an algorithm which assumes the line profile is gaussian and the linewidth is 0.14 nm.

find_offsets_fastdop.pro
idl procedure to read in level 1 file output from fast_doppler_demod.pro and compute 
alignment of images against a single AIA image taken around the same time as the 
9-minute fast doppler sequence. Red and Blue images are averaged together (2 if one beam, 
4 if two beam) x, y and theta offsets are output in an idl sav file with the name 
date.time.ca.sav. 