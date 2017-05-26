; docformat = 'rst'

;+
; Procedure to inventory the contents of all CoMP Level_0 data files
; in a given directory. As a rule all of the images in a CoMP data
; file have a common association. This procedure reads the header
; information in uncompressed CoMP Level_0 files and associates them
; with either 1074, 1079, 1083, dark, opal, cal or distort. File
; inventory information is written into corresponding output
; files. This routine should be run before other data reduction
; routines.
;
; Output files::
;
;   1074_files.txt - file containing information on data files for 1074 region
;   1079_files.txt - file containing information on data files for 1079 region
;   1083_files.txt - file containing information on data files for 1083 region
;   dark_files.txt - file containing information on dark files
;   opal_files.txt - file containing information on opal (flat) files
;   cal_files.txt - file containing information on polarization calibration files
;   dist_files.txt - file containing information on distortion grid files
;
; These files are written to the processed directory for that day and
; copied to the inventory directory.
;
; The output files contain one line for each associated file with the format::
; 
;   filename, integration time, number of data, dark and opal images,
;   shutter status, wavelengths observed, Stokes observed
;
; If no files of a given type were taken on that day, the file will be
; created but left blank.
;
; :Examples:
;   For example, call it like::
;
;     comp_file_type, '20131119'
;
; :Uses:
;   comp_constants_common, comp_config_common, comp_initialize,
;   comp_configuration, comp_inventory, fits_open, fits_close
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;
; :Author:
;   Tomczyk
;
; :History:
;   modified by Sitongia
;   removed file_copy to inventory_dir  Oct 1 2014 GdT
;-
pro comp_file_type, date_dir
  compile_opt idl2
  @comp_constants_common
  @comp_config_common

  ; configure
  comp_initialize, date_dir
  comp_configuration

  mg_log, 'starting', name='comp', /info

  raw_dir = filepath(date_dir, root=raw_basedir)
  cd, raw_dir

  process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)
  file_mkdir, process_dir

  regions = [center1074, center1079, center1083]
  wave_types = ['1074', '1079', '1083']

  openw, lun_1074, filepath('1074_files.txt', root=process_dir), /get_lun
  openw, lun_1079, filepath('1079_files.txt', root=process_dir), /get_lun
  openw, lun_1083, filepath('1083_files.txt', root=process_dir), /get_lun
  openw, lun_dark, filepath('dark_files.txt', root=process_dir), /get_lun
  openw, lun_opal, filepath('opal_files.txt', root=process_dir), /get_lun
  openw, lun_cal, filepath('cal_files.txt', root=process_dir), /get_lun
  openw, lun_distort, filepath('distort_files.txt', root=process_dir), /get_lun

  luns = [lun_1074, lun_1079, lun_1083, $
          lun_dark, lun_opal, lun_cal, lun_distort]

  ; get filenames of all FITS files in this directory
  files = file_search('*.FTS', count=nfile)
  mg_log, '%d raw files', nfile, name='comp', /info

  ; loop over all files
  for i = 0L, nfile - 1L do begin
    fits_open, files[i], fcb, message=error_message
    if (error_message ne '') then begin
      mg_log, 'error reading %s', files[i], name='comp', /error
      mg_log, 'error message: %s', error_message, name='comp', /error
      mg_log, 'skipping %s', files[i], name='comp', /error
      continue
    endif

    ; take inventory
    comp_inventory, fcb, beam, wave, pol, type, expose, cover, cal_pol, cal_ret

    uniq_waves = wave[comp_uniq(wave, sort(wave))]   ; find unique wavelengths
    ; recent failure modes create files with one wavelength - skip these
    if (n_elements(uniq_waves) eq 1 and cover eq 1) then begin
      mg_log, 'bad file: %s', file_basename(files[i]), name='comp', /warn
      fits_close, fcb
      continue
    endif
    uniq_pols = pol[comp_uniq(pol, sort(pol))]   ; find unique polarization states

    ; find wavelength region to associate file
    avg_wave = mean(wave)
    diff = abs(avg_wave - regions)
    mn = min(diff, ireg)

    if (cover eq 1) then begin
      str_cover = '   OPEN  '
    endif else begin
      str_cover = '   CLOSED'
    endelse

    num = fcb.nextend   ; number of images in file
    ndata = 0
    ndark = 0
    nopal = 0
    if (type eq 'DATA') then ndata = num
    if (type eq 'DARK') then ndark = num
    if (type eq 'OPAL') then nopal = num

    ntotal = num

    ifile = ireg
    if (ndark eq ntotal) then ifile = 3   ; if all dark images, write to dark_files
    if (nopal eq ntotal) then ifile = 4   ; if all opal images, write to opal_files
    if (cal_pol eq '1' or cal_ret eq '1') then ifile = 5

    if (ifile lt 3) then begin
      polarizations = strmid(uniq_pols, 2, 1)
      uniq_polarizations = polarizations[uniq(polarizations, sort(polarizations))]
      pol_tag = 'i' + strjoin(strlowcase(uniq_polarizations))
      l1_filename = string(comp_ut_filename(files[i]), wave_types[ifile], $
                           pol_tag, n_elements(uniq_waves), $
                           format='(%"     %s.comp.%d.%s.%d.fts.gz")')
    endif else begin
      l1_filename = ''
    endelse

    mg_log, '%s%s   %7.1f     %5.0f ms   %4d Data   %3d Dark   %3d Opal %s%s%s', $
            files[i], l1_filename, regions[ireg], expose, $
            ndata, ndark, nopal, str_cover, $
            string(uniq_waves, format='(20f9.2)'), $
            string(uniq_pols, format='(20(2x,a))'), $
            name='comp', /debug

    printf, luns[ifile], files[i], l1_filename, expose, ndata, ndark, nopal, str_cover, $
            format='($,a,a,4x,f5.0,1x,"ms",3x,i4," Data",3x,i3," Dark",3x,i3," Opal",a)'

    printf, luns[ifile], format='($,21f9.2)', uniq_waves
    printf, luns[ifile], format='(20(2x,a))', uniq_pols

    fits_close, fcb
  endfor

  free_lun, lun_1074, lun_1079, lun_1083, $
            lun_dark, lun_opal, lun_cal, lun_distort

  mg_log, 'done', name='comp', /info
end

