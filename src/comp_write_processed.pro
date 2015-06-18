; docformat = 'rst'

;+
; Writes CoMP L1 processed data do the specified output file.
;
; :Uses:
;   comp_inventory_header, fits_open, sxdelpar, sxaddpar, fits_write,
;   fits_close
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
;   outfile : in, required, type=string
;     the image and header data are written to this file in fits format.
;
; :Author:
;   Joseph Plowman
;-
pro comp_write_processed, images, headers, primary_header, date_dir, outfile
  compile_opt strictarr

  comp_inventory_header, headers, beam, group, wave, pol, type, expose, $
                         cover, cal_pol, cal_ret

  ; write the input primary header into the output:
  fits_open, outfile, fcbout, /write
  fits_write, fcbout, 0, primary_header

  ; clean up the extension headers and write them to file, along with the
  ; images...
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
    sxdelpar, header, 'LCVR1TMP'
    sxdelpar, header, 'LCVR2TMP'
    sxdelpar, header, 'LCVR3TMP'
    sxdelpar, header, 'LCVR4TMP'
    sxdelpar, header, 'LCVR5TMP'
    sxdelpar, header, 'LCVR6TMP'

    ; add inherit keyword to extension so that readers will get primary and
    ; extension headers merged:
    ;   http://fits.gsfc.nasa.gov/registry/inherit.html
    sxaddpar, header, 'INHERIT', 'T', after='XTENSION'
    
    ; data max and min tags
    sxaddpar, header, 'DATAMIN', min(images[*, *, i]), ' MINIMUM DATA VALUE'
    sxaddpar, header, 'DATAMAX', max(images[*, *, i]), ' MAXIMUM DATA VALUE'

    ename = pol[i] + ', ' + string(format='(f7.2)', wave[i])
    
    fits_write, fcbout, images[*, *, i], header, extname=ename    
  endfor

  fits_close, fcbout
end


