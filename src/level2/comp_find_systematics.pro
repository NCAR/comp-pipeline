; docformat = 'rst'

;+
; Procedure to quantify and plot systematic errors in comp data. Typically,
; 'mean' is input as the file_type and this routine reads the averaged data
; produced by `COMP_AVERAGE`. It computes and plots histograms showing
; systematic offsets in the data. Plots of the histograms are saved in the
; engineering directory.
;
; Plots of histograms showing systematic errors are saved as .ps files in the
; engineering directory.
;
; :Examples:
;   For example, call like::
;
;     find_systematics, '20130103', '1074', 'mean'
;
; :Uses:
;   comp_constants_common, comp_config_common, fits_open, fits_read, sxpar,
;   mg_log
;
; :Params:
;   date_dir : in, required, type=string
;     date to process, in YYYYMMDD format
;   wave_type : in, required, type=string
;     wavelength range for the observations, '1074', '1079' or '1083'
;   file_type : in, required, type=string
;     unique part of filename to select input file, such as 'mean'
;
; :Keywords:
;   error : out, optional, type=long
;     set to a named variable to return the error status of the routine, 0 for
;     success, anything else for failure
;
; :Author:
;   MLSO Software Team
;
; :History:
;   removed gzip    Oct 1 2014  GdT
;   removed copy_file to engineering_dir  Oct 1 2014  GdT
;   see git log for recent changes
;-
pro comp_find_systematics, date_dir, wave_type, file_type, error=error, synoptic=synoptic
  compile_opt idl2
  @comp_constants_common
  @comp_config_common

  mg_log, 'find systematics %s', wave_type, name='comp', /info

  ; Establish error handler. When errors occur, the index of the
  ; error is returned in the variable Error_status:
  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    mg_log, /last_error, name='comp'
    return
  endif

  l1_process_dir = filepath('', subdir=[date_dir, 'level1'], root=process_basedir)
  l2_process_dir = filepath('', subdir=[date_dir, 'level2'], root=process_basedir)
  cd, l2_process_dir

  ; save plots
  eng_dir = filepath('', subdir=comp_decompose_date(date_dir), root=engineering_dir)
  if (~file_test(eng_dir, /directory)) then file_mkdir, eng_dir

  type = keyword_set(synoptic) ? 'synoptic' : 'waves'
  file_dir = string(date_dir, wave_type, file_type, type, $
                    format='(%"%s.comp.%s.%s.%s")')
  filename = file_dir + '.fts.gz'

  if (~file_test(filename) || file_test(filename, /zero_length)) then begin
    mg_log, '%s does not exist', filename, name='comp', /warn
    goto, done
  endif

  fits_open, filename, fcb   ; open input file
  fits_read, fcb, data, header, /header_only, exten_no=0, /no_abort, message=msg
  if (msg ne '') then begin
    mg_log, 'problem reading %s', filename, name='comp', /error
    fits_close, fcb
    message, msg
  endif

  mg_log, 'reading %s', file_basename(filename), name='comp', /debug

  nwave = sxpar(header, 'NTUNES')

  ; number of images in file (not background)
  ndat = fcb.nextend - nwave
  dat = fltarr(nx, nx, ndat)
  wav = strarr(ndat)
  pol = strarr(ndat)

  ; read in all images
  for i = 0L, ndat - 1L do begin
    fits_read, fcb, d, header, exten_no=i + 1, /no_abort, message=msg
    if (msg ne '') then message, msg
    dat[*, *, i] = d
    wav[i] = sxpar(header, 'WAVELENG')
    pol[i] = sxpar(header, 'POLSTATE')
  endfor
  fits_close, fcb

  for i = 0L, ndat - 1L do begin
    mg_log, 'histogram for extension %d/%d', i + 1, ndat, name='comp', /debug
    d = dat[*, *, i]
    if (i gt nwave * 3 - 1) then begin   ; if ext is V data
      dat[*, *, i] += dat[*, *, i - nwave] * 0.185
    endif

    good = where(d ne 0., count)
    if (count eq 0) then begin
      mg_log, 'ext %d all 0.0 (%s @ %0.2f nm), skipping', $
              i + 1, strtrim(pol[i], 2), wav[i], $
              name='comp', /warn
      continue
    endif

    if (i lt nwave) then bs = 0.01 else bs = 0.001
    histogram_xrange = i lt nwave ? [-5., 10.] : [-0.4, 0.4]
    h = histogram(d[good], binsize=bs, locations=x, $
                  min=histogram_xrange[0], max=histogram_xrange[1])

    mo = moment(d[good])
    cutoff = 5000.0
    mo0_format = abs(mo[0]) gt cutoff ? '(%"%0.2e")' : '(%"%0.2f")'
    mo0 = string(mo[0], format=mo0_format)
    mo1_format = abs(mo[1]) gt cutoff ? '(%"%0.2e")' : '(%"%0.2f")'
    mo1 = string(mo[1], format=mo1_format)

    histogram_plot = plot(x, h, $
                          title=string(wav[i], pol[i], mo0, mo1, $
                                       format='(%"%4d %1s, %s, %s")'), $
                          font_size=4, $
                          xrange=histogram_xrange, $
                          yrange=[0, 1.2 * max(h)], $
                          layout=[nwave, (ndat + nwave - 1) / nwave, i + 1], $
                          margin=[0.3, 0.1, 0.1, 0.1], $
                          /current, $
                          /buffer)
  endfor

  if (obj_valid(histogram_plot)) then begin
    histogram_plot->save, filepath(file_dir + '.his.gif', root=eng_dir), $
                          /landscape, xmargin=1.0, ymargin=0.0, $
                          width=19.0
    histogram_plot->close
    mg_log, 'wrote histogram plot', name='comp', /info
  endif else begin
    mg_log, 'no nonzero images to plot', name='comp', /warn
  endelse

  ; plot images

  for i = 0L, ndat - 1L do begin
    mg_log, 'image for extension %d/%d', i + 1, ndat, name='comp', /debug

    d = dat[*, *, i]
    d -= mean(d[good])

    case fix(i / nwave) of
      0: begin
          xmin = 0.0
          xmax = 5.0
        end
      1: begin
          xmin = -0.25
          xmax = 0.25
        end
      2: begin
          xmin = -0.25
          xmax = 0.25
        end
      3: begin
          xmin = -0.1
          xmax = 0.1
        end
    endcase

    dd = bytscl(d, xmin, xmax)

    im = image(rebin(dd, nx / 2, nx / 2), $
               layout=[nwave, (ndat + nwave - 1) / nwave, i + 1], $
               /current, $
               /buffer, $
               margin=0)
  endfor

  im->save, filepath(file_dir + '.img.gif', root=eng_dir), $
            /landscape, xmargin=0, width=10.5, ymargin=0
  im->close
  mg_log, 'wrote images', name='comp', /info

  ; plot correlation only if Stokes V was observed
  if (ndat gt 3 * nwave) then begin
    cor = dat[*, *, 2]
    good = where(cor ne 0., count)
    if (count eq 0) then begin
      mg_log, 'no good correlation', name='comp', /warn
    endif else begin
      i_index = nwave / 2 - 1
      i = dat[*, *, i_index]
      v_index = nwave * 3 + nwave / 2 - 1
      v = dat[*, *, v_index]

      scatter_plot = plot(i[good], v[good], $
                          title='I,V Correlation', $
                          xtitle='I Center', $
                          ytitle='V Center', $
                          linestyle='none', $
                          symbol='.', $
                          /current, $
                          /buffer)

      coef = poly_fit(i[good], v[good], 1)

      x = findgen(100) * 0.01 * (max(i) - min(i)) + min(i)
      y = poly(x, coef)

      corr_plot = plot(x, y, /overplot, /buffer)
      t = text(.6, .8, string(format='("Cor 0: ",f8.4)', coef[0]))
      t = text(.6, .75, string(format='("Cor 1: ",f8.4)', coef[1]))

      scatter_plot->save, filepath(file_dir + '.sca.gif', root=eng_dir), $
                          xmargin=0, ymargin=0, /bitmap
      scatter_plot->close
      mg_log, 'wrote correlation plots', name='comp', /info
    endelse
  endif else begin
    mg_log, 'not plotting correlation because no V observed', name='comp', /info
  endelse

  done:
  fits_close, fcb

  mg_log, 'done', name='comp', /info
end
