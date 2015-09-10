; docformat = 'rst'

;+
; Procedure to extract CoMP intensity images from all of the Level_1 files in
; a directory. The intensity image is defined as the Stokes I image closest to
; line center. This routine outputs the intensity images to FITS files and also
; creates GIF images.
;
; The intensity image extracted from each file is written to an output FITS
; files with the name date.time.comp.wwww.intensity.fts where date and time are
; from the original file and wwww is the wave_type.
;
; In addition, gif images are output with the filenames
; date.time.comp.wwww.intensity.gif
;
; The files are written to the process directory.
;
; :Examples:
;   For example, call it like::
;
;     comp_extract_intensity, '20121209', '1074'
;
; :Uses:
;   comp_constants_common, comp_paths_common, comp_make_gif, fits_open,
;   fits_read, fits_write, fits_close, sxaddpar
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;
; :Keywords:
;   error : out, optional, type=long
;     set to a named variable to return the error status of the routine, 0 for
;     success, anything else for failure
;
; :Author: Sitongia
;
; :Modification History:
;   removed gzip Oct 1 2014  GdT
;-
pro comp_extract_intensity, date_dir, wave_type, error=error
  compile_opt idl2
  @comp_constants_common
  @comp_config_common

  ; configure
  comp_initialize, date_dir
  comp_paths

  mg_log, 'wave_type: %s', wave_type, name='comp', /info

  ; Establish error handler. When errors occur, the index of the
  ; error is returned in the variable Error_status:
  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    mg_log, /last_error, name='comp'
    return
  endif

  ; check keywords
  uselist = 0
  if n_elements(list_file) eq 1 then uselist = 1

  process_dir = filepath(date_dir, root=process_basedir)
  cd, process_dir

  ; line center
  case wave_type of
    '1074' : begin
        lineCenter = center1074
        ScaleMax = 20.0
        gifMin = 0.0
        gifMax = 5.0
      end
    '1079' : begin
        lineCenter = center1079
        ScaleMax = 20.0
        gifMin = 0
        gifMax = 3.5
      end
    '1083' : begin
        lineCenter = center1083
        ScaleMax = 200000.0
        gifMin = 0
        gifMax = 12.0
      end
  endcase

  ; the FITS image files
  files = file_search('*.comp.' + wave_type + '.fts', COUNT=count)

  for filesIndex = 0L, count - 1L do begin
    mg_log, 'file %s', files[filesIndex], name='comp', /debug

    fits_read, files[filesIndex], data, primary_header, exten_no=0
    fits_read, files[filesIndex], data, header, exten_no=1

    comp_extract_intensity_cube, files[filesIndex], images, waves

    ; determine index of wavelength closest to line center
    waveDiff = abs(waves - lineCenter)
    lineCenterIndex = where(waveDiff eq min(waveDiff))
    lineCenterIndex = lineCenterIndex[0]
    mg_log, 'using wavelength %f', waves[lineCenterIndex], name='comp', /debug

    ; intensity simple by extracting image near line center
    intensity = images[*, *, lineCenterIndex]
    ; clip
    ; intensity = 0 > intensity < ScaleMax ; larger of 0 and image, then smaller of image and ScaleMax.
    ; set the processing level
    sxaddpar, primary_header, 'LEVEL   ', 'L1'
    ; write files
    sxaddpar, primary_header, 'METHOD  ', 'EXTRACT', $
              ' Method used: extract filtergram at line center'

    fits_open, string(strmid(files[filesIndex], 0, 15), wave_type, $
                      format='(%"%s.comp.%s.intensity.fts")'), $
               fcbout, /write
    fits_write, fcbout, 0, primary_header
    sxaddpar, header, 'WAVELENG', waves[lineCenterIndex]
    fits_write, fcbout, intensity, header, $
                extname=string(waves[lineCenterIndex], format='(f7.2)')
    fits_close, fcbout
  endfor

  ; compress files
  ;if debug eq 1 then print, "Compressing FITS files."
  ;spawn, 'gzip -f *.comp.' + wave_type + '.intensity.fts'

  ; make GIF files from Intensity
  files = file_search('*.comp.' + wave_type + '.intensity.fts', count=count)
  mg_log, 'make GIFs', name='comp', /debug
  for filesIndex = 0L, count - 1L do begin
    ; make GIF from I file
    comp_make_gif, date_dir, files[filesIndex], nx, 'Intensity', wave_type, $
                   gifMin, gifMax
  endfor

  mg_log, 'done', name='comp', /info
end
