; docformat = 'rst'

;+
; Writes CoMP L1 processed data do the specified output file.
;
; :Uses:
;   comp_set_background, comp_inventory_header, comp_make_mask,
;   fits_open, sxdelpar, sxaddpar, fits_write, fits_close
;
; :Params:
;   images : in, required, type="fltarr(nx, ny, nimg)"
;     an array of L1 processed images
;   headers : in, required, type="strarr(ntags, nimg)"
;     the extension headers, one for each image in the array
;   primary_header : in, required, type = strarr(nptags)
;     the primary header for the file
;   date_dir : in, required, type=string
;     the directory for containing the files for the date in question, used to
;     find the flat file
;   filename : in, required, type=string
;     filename of corresponding L0 file
;   wave_type : in, required, type=string
;     wavelength to process, '1074', '1079', etc.
;
; :Author:
;   Joseph Plowman
;-
pro comp_write_processed, images, headers, primary_header, date_dir, filename, $
                          wave_type
  compile_opt strictarr
  @comp_config_common
  @comp_constants_common

  comp_set_background, date_dir, primary_header, images, headers

  ; will need mask for BACKGRND in extensions
  comp_make_mask, date_dir, primary_header, mask

  comp_inventory_header, headers, beam, group, wavelengths, polarizations, $
                         type, expose, $
                         cover, cal_pol, cal_ret

  ; create output filename of the form:
  ;   [date].[time].comp.[central wavelength].[polarization states].[n wavelengths].fts

  unique_wavelengths = wavelengths[uniq(wavelengths, sort(wavelengths))]
  n_wavelengths = n_elements(unique_wavelengths)

  unique_polarizations = polarizations[uniq(polarizations, sort(polarizations))]
  unique_polarizations = unique_polarizations[where(strmid(unique_polarizations, 0, 3) ne 'BKG')]
  polarization_tag = strlowcase(strjoin(unique_polarizations))

  datetime = strmid(file_basename(filename), 0, 15)
  output_filename = filepath(string(comp_ut_filename(datetime), $
                                    wave_type, $
                                    polarization_tag, $
                                    n_wavelengths, $
                                    format='(%"%s.comp.%s.%s.%d.fts")'), $
                             subdir=[date_dir, 'level1'], $
                             root=process_basedir)
  background_filename = filepath(string(comp_ut_filename(datetime), $
                                        wave_type, $
                                        polarization_tag, $
                                        n_wavelengths, $
                                        format='(%"%s.comp.%s.%s.%d.bkg.fts")'), $
                                 subdir=[date_dir, 'level1'], $
                                 root=process_basedir)

  ; write the input primary header into the output
  fits_open, output_filename, fcb_out, /write
  fits_write, fcb_out, 0.0, primary_header

  fits_open, background_filename, fcb_back, /write
  fits_write, fcb_back, 0.0, primary_header

  ; clean up the extension headers and write them to file, along with the
  ; images
  nexts = n_elements(headers[0, *])    ; number of images
  ntags0 = n_elements(headers[*, 0])   ; number of total tags

  ; find out how where the extension header begins
  for i = 0L, ntags0 - 1L do begin
    if (strcmp(headers[i, 0], 'BEGIN EXTENSION HEADER', 20L)) then break
  endfor

  iexten = i + 1
  for i = 0L, nexts - 1L do begin
    header = headers[iexten:ntags0 - 1, i]

    ; clean up extension header before using it in writing
    sxdelpar, header, 'COMMENT'
    sxdelpar, header, 'SEQUENCE'
    sxdelpar, header, 'LCVR1VOL'
    sxdelpar, header, 'LCVR2VOL'
    sxdelpar, header, 'LCVR3VOL'
    sxdelpar, header, 'LCVR4VOL'
    sxdelpar, header, 'LCVR5VOL'
    sxdelpar, header, 'LCVR6VOL'

    sxaddpar, header, 'NAXIS1', sxpar(header, 'NAXIS1'), ' (pixels) x dimension'
    sxaddpar, header, 'NAXIS2', sxpar(header, 'NAXIS2'), ' (pixels) y dimension'

    sxaddpar, header, 'PCOUNT', sxpar(header, 'PCOUNT'), $
              ' FITS definition: an integer = num. parameters preceding each array'
    sxaddpar, header, 'GCOUNT', sxpar(header, 'GCOUNT'), $
              ' FITS definition: an integer = num. of random groups present.'

    sxaddpar, header, 'EXPOSURE', sxpar(header, 'EXPOSURE'), $
              format='(F0.2)'
    sxaddpar, header, 'BODYTEMP', sxpar(header, 'BODYTEMP'), $
              format='(F0.3)'
    sxaddpar, header, 'BASETEMP', sxpar(header, 'BASETEMP'), $
              format='(F0.3)'
    sxaddpar, header, 'RACKTEMP', sxpar(header, 'RACKTEMP'), $
              format='(F0.3)'
    sxaddpar, header, 'OPTRTEMP', sxpar(header, 'OPTRTEMP'), $
              format='(F0.3)'
    sxaddpar, header, 'FILTTEMP', sxpar(header, 'FILTTEMP'), $
              format='(F0.3)'

    ndfilter = sxpar(header, 'NDFILTER', count=n_ndfilter)
    if (n_ndfilter eq 0) then begin
      sxaddpar, header, 'NDFILTER', 8, $
                ' ND 1=.1, 2=.3, 3=.5, 4=1, 5=2, 6=3, 7=4, 8=cle', after='LCVR6TMP'
    endif

    ; add inherit keyword to extension so that readers will get primary and
    ; extension headers merged:
    ;   http://fits.gsfc.nasa.gov/registry/inherit.html
    sxaddpar, header, 'INHERIT', 'T', after='XTENSION'

    ename = polarizations[i] + ', ' + string(format='(f7.2)', wavelengths[i])

    case wave_type of
      '1074': begin
          dispmin = dispmin1074
          dispmax = dispmax1074
          dispexp = dispexp1074
        end
      '1079': begin
          dispmin = dispmin1079
          dispmax = dispmax1079
          dispexp = dispexp1079
        end
      '1083': begin
          dispmin = dispmin1083
          dispmax = dispmax1083
          dispexp = dispexp1083
        end
    endcase

    sxaddpar, header, 'DISPMIN', dispmin, ' Minimum data value', format='(F0.2)'
    sxaddpar, header, 'DISPMAX', dispmax, ' Maximum data value', format='(F0.2)'
    sxaddpar, header, 'DISPEXP', dispexp, ' Exponent value for scaling', format='(F0.2)'

    ; this assumes that all the background extensions are last
    if (strmid(ename, 0, 3) eq 'BKG') then begin
      ; the foreground wavelength is not correct for the background
      sxaddpar, header, 'WAVELENG', $
                strjoin(string(sxpar(header, 'WAVELENG'), format='(F0.2)') $
                          + string(0.57 * [-1, 1], format='(F+0.2)'), ','), $
                ' Blue and red continuum [nm]'
      fits_write, fcb_back, images[*, *, i], header, extname=ename
    endif else begin
      ; give a median background for each extension
      background = comp_get_component(images, headers, $
                                      'BKG' + polarizations[i], $
                                      0, $
                                      wavelengths[i])
      extension_background = median(background[where(mask eq 1.0)])
      sxaddpar, header, 'BACKGRND', extension_background, $
                ' Median of masked line center background', format='(F10.3)', $
                after='ND-TRANS'

      fits_write, fcb_out, images[*, *, i], header, extname=ename
    endelse
  endfor

  fits_close, fcb_out
  fits_close, fcb_back
end
