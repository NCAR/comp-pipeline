; docformat = 'rst'

;+
; Procedure to extract CoMP intensity images from all of the Level 1 files in
; a directory. The intensity image is defined as the Stokes I image closest to
; line center. This routine outputs GIF images.
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
;   background : in, optional, type=boolean
;     set to extract center wavelength intensity from background images
;
; :Author:
;   MLSO Software Team
;
; :History:
;   removed gzip Oct 1 2014  GdT
;   see git log for recent changes
;-
pro comp_extract_intensity, date_dir, wave_type, error=error, background=background
  compile_opt idl2
  @comp_constants_common
  @comp_config_common

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

  l1_process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)
  cd, l1_process_dir

  ; line center
  case wave_type of
    '1074' : begin
        line_center = center1074
        scale_max = 20.0
      end
    '1079' : begin
        line_center = center1079
        scale_max = 20.0
      end
    '1083' : begin
        line_center = center1083
        scale_max = 200000.0
      end
  endcase

  ; the FITS image file
  files = comp_find_l1_file(date_dir, wave_type, /all, count=n_files, background=background)
  mg_log, 'found %d files for %s', n_files, wave_type, name='comp', /info

  for f = 0L, n_files - 1L do begin
    mg_log, '%d/%d: %s', f + 1L, n_files, file_basename(files[f]), name='comp', /debug

    comp_extract_intensity_cube, files[f], $
                                 images=images, $
                                 wavelengths=wavelengths, $
                                 primary_header=primary_header, $
                                 headers=headers, $
                                 background=background

    nd_filter = comp_get_nd_filter(date_dir, wave_type, headers[*, 0])
    if (wave_type eq '1083' && nd_filter eq 8) then begin
      mg_log, 'skipping %s image with ND=%d', $
              wave_type, nd_filter, name='comp', /info
      continue
    endif

    ; determine index of wavelength closest to line center
    wave_diff = abs(wavelengths - line_center)
    !null = min(wave_diff, line_center_index)
    mg_log, 'using wavelength %0.2f', wavelengths[line_center_index], $
            name='comp', /debug

    ; intensity simple by extracting image near line center
    intensity = images[*, *, line_center_index]

    ; clip intensity = 0 > intensity < scale_max; larger of 0 and
    ; image, then smaller of image and scale_max

    ; make GIF from I
    if (keyword_set(background)) then begin
      output_filename = string(strmid(file_basename(files[f]), 0, 15), wave_type, $
                               format='(%"%s.comp.%s.intensity.bkg.gif")')
    endif else begin
      output_filename = string(strmid(file_basename(files[f]), 0, 15), wave_type, $
                               format='(%"%s.comp.%s.intensity.gif")')
    endelse
    comp_make_gif, date_dir, intensity, primary_header, output_filename, $
                   nx, $
                   keyword_set(background) ? 'Background' : 'Intensity', $
                   wave_type, background=background
  endfor

  mg_log, 'done', name='comp', /info
end
