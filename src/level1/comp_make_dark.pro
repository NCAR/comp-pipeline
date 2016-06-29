; docformat = 'rst'

;+
; Procedure to read CoMP uncompressed Level_0 dark images for a day and create
; one file with averaged dark images. Typically, a sequence of dark images is
; taken and written to a file several times each day. This routine averages
; together all dark images from each dark file and writes the average to a
; FITS file. This routine reads the `dark_files.txt` file that was created by
; `file_type`.
;
; File `dark.fts` is written to the process directory and a copy is written to
; the `bias` directory. Each extension of the `dark.fts` file contains an
; average dark image corresponding to a dark file. There is also a 1d
; extension written to `dark.fts` that contains the time the darks were taken,
; and another 1d extension written containing the exposure time for the darks.
; Then there will be n+2 extensions to the dark.fts file, corresponding to `n`
; dark files taken that day plus the time and exposure extensions.
;
; :Examples:
;   For example, call it like::
;
;     make_dark, '20130915'
;
; :Uses:
;   comp_constants_common, comp_config_common, comp_initialize,
;   comp_configuration, comp_demultiplex, fits_open, fits_read, fits_close,
;   fits_write, sxdelpar, sxpar
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;
; :Keywords:
;   error : out, optional, type=long
;     set to a named variable to return the error status of the routine, 0 for
;     success, anything else for failure
;
; :Author:
;   Tomczyk, modified by Sitongia
;
; :History:
;   removed file_copy to biar_dir  Oct 2 2014   GdT
;-
pro comp_make_dark, date_dir, error=error
  compile_opt idl2
  @comp_constants_common
  @comp_config_common

  ; configure
  comp_initialize, date_dir
  comp_configuration

  mg_log, 'starting', name='comp', /info

  ; Establish error handler. When errors occur, the index of the
  ; error is returned in the variable Error_status:
  catch, error
  if (error ne 0) then begin
    catch, /cancel
    mg_log, /last_error, name='comp'
    return
  endif

  raw_dir = filepath(date_dir, root=raw_basedir)
  process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)
  if (~file_test(process_dir, /directory)) then file_mkdir, process_dir
  cd, process_dir

  ;  open list of dark images
  openr, dark_lun, 'dark_files.txt', /get_lun

  ;  open output fits file
  fits_open, 'dark.fts', fcbout, /write
  nout = 0
  time = fltarr(1000)
  exposure = fltarr(1000)

  ;  loop through dark files and average dark images
  while (not eof(dark_lun)) do begin
    darkfile = ' '
    readf, dark_lun, darkfile, format='(a19)'

    fits_open, filepath(darkfile, root=raw_dir), fcb
    num = fcb.nextend
    fits_read, fcb, dat, primary_header, /header_only, exten_no=0
    time_str = sxpar(primary_header, 'TIME_OBS')

    ; write the primary header of the first dark file as the primary header of
    ; the output file
    if (nout eq 0) then begin
      sxdelpar, primary_header, 'TIME_OBS'
      sxaddpar, primary_header, 'LEVEL','L1'
      sxaddpar, primary_header, 'VERSION', code_version, $
                ' Calibration processing software version'
      sxaddpar, primary_header, 'REVISION', code_revision, $
                ' Calibration processing software revision'
      fits_write, fcbout, 0, primary_header
    endif

    --num   ; skip first image (which is often bad)

    images = fltarr(1024, 1024, num)

    mean_dark = fltarr(num)

    for i = 0L, num - 1L do begin
      fits_read, fcb, dat, header, exten_no=i + 2
      if (sxpar(header, 'DEMULT') eq 0) then begin
        dat = comp_demultiplex(dat)
      endif
      dat = float(dat)
      images[*, *, i] = dat

      mean_dark[i] = mean(dat)
    endfor

    fits_close, fcb

    ; use good darks only
    ; good darks are defined as those not differing from the median by more than 2 ADU

    good = where(abs(mean_dark - median(mean_dark)) lt 2., count)
    if (count lt num) then begin
      mg_log, 'good: %s', strjoin(strtrim(good, 2), ', '), name='comp', /debug
    endif

    aver = mean(mean_dark[good])
    if (count gt 1) then begin
      noise = stdev(mean_dark[good])
    endif else begin
      noise = 0
    endelse
    dark = fltarr(1024,1024)
    for i = 0L, count - 1L do dark += images[*, *, good[i]]
    dark = dark / float(count)

    ; compute noise in 20x20 subarray

    nsub = 20
    sigma = fltarr(nsub, nsub)
    if (count gt 1) then begin
      for i = 0L, nsub - 1L do begin
        for j = 0L, nsub - 1L do begin
          sigma[i, j] = stdev(images[i + 128, j + 128, good])
        endfor
      endfor
    endif

    ; write out dark
    sxdelpar, header, 'POLSTATE'
    sxdelpar, header, 'SEQUENCE'
    sxaddpar, header, 'DATATYPE','DARK'
    sxaddpar, header, 'DEMULT', 1

    time[nout] = comp_parse_time(time_str)

    exposure[nout] = sxpar(header, 'EXPOSURE')
    sxaddpar, header, 'TIME', time[nout]
    sxaddpar, header, 'MEAN', aver
    sxaddpar, header, 'NOISE', median(sigma)

    mg_log, '%s: %5.1fms, %d/%d used, mean: %0.1f, rms: %0.3f', $
            comp_times2str(time[nout]), $
            exposure[nout], $
            count, num, $
            aver, noise, $
            name='comp', /info

    ++nout

    ; transfer other primary header values into this header
    sxaddpar, header, 'TIME_OBS', time_str

    nans = where(finite(dark, /nan), count)
    if (count gt 0) then begin
      mg_log, 'NaNs in %s', darkfile, name='comp', /warn
    endif
    fits_write, fcbout, dark, header
  endwhile

  ; write out arrays with times and exposures
  time = time[0L:nout - 1L]
  sxaddpar, header, 'DATATYPE', 'TIME'
  fits_write, fcbout, time, header, extname='Time'

  exposure = exposure[0:nout - 1L]
  sxaddpar, header, 'DATATYPE', 'EXPOSURE'
  fits_write, fcbout, exposure, header, extname='Exposure'

  fits_close, fcbout
  free_lun, dark_lun

  mg_log, 'done', name='comp', /info
end
