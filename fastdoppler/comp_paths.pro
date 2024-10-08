;+
; :Description:
;    Set up paths for CoMP pipeline.
;
; :Params:
;    date_dir
;
;
;
; :Author: sitongia
;-
pro comp_paths, date_dir

common comp_paths, bias_dir, flat_dir, mask_dir, binary_dir, $
  defered_file, hot_file, $
  ldm_basedir, raw_basedir, process_basedir, hpss_gateway, $
  archive_dir, movie_dir, fullres_dir, log_dir, $
  ffmpeg_dir, logo_dir

  if getenv('USER') eq 'tomczyk' then begin

; Calibration files
    bias_dir   = 'X:\CoMP\bias\'
    flat_dir   = 'X:\CoMP\flat\'
    mask_dir   = 'X:\CoMP\mask\'

; Binary files
    binary_dir = 'C:\Documents and Settings\tomczyk\My Documents\MLSO\CoMP\Pipeline\'

; Processing
    ldm_basedir     =      ''
    raw_basedir     = 'V:\Data\CoMP\raw\'
    process_basedir = 'E:\CoMP\process\'

; Distribution of results
    archive_dir = 'E:\CoMP\process\'
    movie_dir   = 'E:\CoMP\'
    fullres_dir = 'E:\CoMP\'

; Log files
    log_dir = 'E:CoMP\Logs\'

; Dynamics and Polarization
    ffmpeg_dir = 'C:\Program Files\ffmpeg\bin'
    logo_dir = 'C:\Users\sitongia\Projects\CoMP\Pipeline\dynamics\logos\'

  endif

  if getenv('USER') eq 'zihao' then begin

; Calibration files
    bias_dir   = ''
    flat_dir   = ''
    mask_dir   = ''

; Binary files
    binary_dir = ''

; Processing
    ldm_basedir     =      ''
    raw_basedir     =      '/Data/wave/raw/';
    ;raw_basedir     =      '/Volumes/E/wave/raw/'
    process_basedir =      '/Data/wave/process/';
    ;process_basedir =      '/Volumes/E/wave/process/'

; Distribution of results
    archive_dir = ''
    movie_dir   = ''
    fullres_dir = ''

; Log files
    log_dir = '/Data/wave/logs/';
    ;log_dir = '/Volumes/E/wave/logs/'

; Dynamics and Polarization
    ffmpeg_dir = ''
    logo_dir = ''

  endif

  hot_file = binary_dir + 'hothot.sav'
  defered_file = binary_dir + 'deferred.sav'

end
