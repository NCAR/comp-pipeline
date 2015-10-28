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
;   comp_constants_common, comp_config_common, comp_make_gif, fits_open,
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
  comp_configuration

  mg_log, 'wave_type: %s', wave_type, name='comp', /info

  ; establish error handler; when errors occur, the index of the
  ; error is returned in the variable error
  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    mg_log, /last_error, name='comp'
    return
  endif

  ; check keywords
  uselist = n_elements(list_file) eq 1L

  process_dir = filepath(date_dir, root=process_basedir)
  cd, process_dir

  ; line center
  case wave_type of
    '1074' : begin
        line_center = center1074
        scale_max = 20.0
        display_min = 0.0
        display_max = 5.0
      end
    '1079' : begin
        line_center = center1079
        scale_max = 20.0
        display_min = 0
        display_max = 3.5
      end
    '1083' : begin
        line_center = center1083
        scale_max = 200000.0
        display_min = 0
        display_max = 12.0
      end
  endcase

  ; the FITS image file
  search_filter = '*.comp.' + wave_type + '.[iquv]*.[1-9]{,[1-9]}.fts'
  files = file_search(search_filter, count=n_files)
  mg_log, 'found %d files for %s', n_files, wave_type, name='comp', /info

  for f = 0L, n_files - 1L do begin
    mg_log, 'file %s', files[f], name='comp', /debug

    fits_read, files[f], data, primary_header, exten_no=0
    fits_read, files[f], data, header, exten_no=1

    comp_extract_intensity_cube, files[f], images, waves

    ; determine index of wavelength closest to line center
    wave_diff = abs(waves - line_center)
    line_center_index = where(wave_diff eq min(wave_diff))
    line_center_index = line_center_index[0]
    mg_log, 'using wavelength %f', waves[line_center_index], name='comp', /debug

    ; intensity simple by extracting image near line center
    intensity = images[*, *, line_center_index]

    ; clip intensity = 0 > intensity < scale_max; larger of 0 and
    ; image, then smaller of image and scale_max

    ; only create 5 wavelength 1083 FITS files
    dims = size(images, /dimensions)
    if (wave_type eq '1083' && n_elements(waves) eq 5) then begin
      ; set the processing level
      sxaddpar, primary_header, 'LEVEL   ', 'L1'

      ; write files
      sxaddpar, primary_header, 'METHOD  ', 'EXTRACT', $
                ' Method used: extract filtergram at line center'

      fits_open, string(strmid(files[f], 0, 15), wave_type, $
                        format='(%"%s.comp.%s.intensity.fts")'), $
                 fcbout, /write
      fits_write, fcbout, 0, primary_header
      sxaddpar, header, 'WAVELENG', waves[line_center_index]
      fits_write, fcbout, intensity, header, $
                  extname=string(waves[line_center_index], format='(f7.2)')
      fits_close, fcbout
    endif

    ; make GIF from I
    output_filename = string(strmid(files[f], 0, 15), wave_type, $
                             format='(%"%s.comp.%s.intensity.gif")')
    comp_make_gif, date_dir, intensity, primary_header, output_filename, $
                   nx, 'Intensity', wave_type, display_min, display_max
  endfor

  mg_log, 'done', name='comp', /info
end
