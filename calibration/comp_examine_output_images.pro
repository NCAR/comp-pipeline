; find statistics for masked annulus matching beam state

cal_struct_sav_filename = filepath('calibration_structure_wtrans.sav', $
                                   subdir=['..', 'cal_results'], $
                                   root=mg_src_root())
restore, filename=cal_struct_sav_filename

retarder = [0B, 1B]
pol_angle = [0, 45, 90, 135]

stokes_labels = ['I', 'Q', 'U', 'V']
root = '/export/data1/Data/CoMP/process.calibration/20150729'

original_device = !d.name
set_plot, 'ps'

;window, xsize=n_elements(stokes_labels) * 256, ysize=n_elements(retarder) * n_elements(pol_angle) * 128
device, filename='plots.ps', xsize=6.5, ysize=8.0, /inches, yoffset=2.0

!p.multi = [0, $
            n_elements(stokes_labels), $
            n_elements(retarder) * n_elements(pol_angle), $
            0, $
            0]
for r = 0L, n_elements(retarder) - 1L do begin
  for pa = 0L, n_elements(pol_angle) - 1L do begin
    subdir = string(retarder[r] ? 'in' : 'out', pol_angle[pa], format='(%"ret-%s.pol-%d")')

    filenames = filepath('*.sav', subdir=subdir, root=root)

    files = file_search(filenames)

    filename = files[0]   ; just pick a single wavelength and beam state
    basename = file_basename(filename)

    re = '[[:digit:]]{8}\.([[:digit:]]{6})-([[:digit:]]{6})\.FTS-([[:digit:].]{7})-([[:digit:]])\.sav'
    re_matches = stregex(basename, re, /extract, /subexpr)
    qu_time = re_matches[1]
    v_time = re_matches[2]
    wavelength = float(re_matches[3])
    beam_state = long(re_matches[4])

    restore, filename=filename

    mask = cal_struct.mask * (beam_state eq 0 ? cal_struct.lowermask : cal_struct_uppermask)
    !null = where(mask, n_pixels)
    print, retarder[r] ? 'in' : 'out', pol_angle[pa], $
           format='(%"retarder: %s, pol_angle: %0.1f degrees")'
    for p = 0L, n_elements(stokes_labels) - 1L do begin
      ind = where(mask, count)
      im = (reform(pols_images[*, *, p]))[ind]
      print, stokes_labels[p], mean(im), median(im), stddev(im), mg_range(im), $
             format='(%"  %s mean: %0.3f, median: %0.3f, std dev: %0.3f, range: %0.3f to %0.3f")'
      h = histogram(im, min=-1.0, max=1.0, nbins=2000, locations=locations)
      xrange = mg_range(im)
      xrange = [3.0 * xrange[0] - xrange[1], 3.0 * xrange[1] - xrange[0]] / 2.0
      plot, locations, h, xstyle=9, ystyle=9, psym=10, charsize=1.0, $
            xrange=xrange, yrange=[0.0, n_pixels], xticks=2, $
            title=r eq 0 && pa eq 0 ? stokes_labels[p] : '', $
            ytitle=p eq 0 ? string(retarder[r] ? 'in' : 'out', pol_angle[pa], $
                                   format='(%"%s : %0.1f")') : ''
;      xyouts, xrange[1], n_pixels, $
;              string(r eq 0 && pa eq 0 ? stokes_labels[p] : '', $
;                     p eq 0 ? (retarder[r] ? 'in' : 'out') : '', $
;                     p eq 0 ? string(pol_angle[pa], format='(%"%0.1f")') : '', $
;                     format='(%"%s!C%s!C%s")'), $
;              /data, alignment=1.0, charsize=0.6
      xyouts, xrange[1], n_pixels, $
              string(mean(im), median(im), stddev(im), mg_range(im), $
                     format='(%"!Cmean: %0.3f!Cmedian: %0.3f!Cstd dev: %0.3f!Crange: %0.3f to %0.3f")'), $
              /data, alignment=1.0, charsize=0.5
    endfor
  endfor
endfor

!p.multi = 0
device, /close_file
set_plot, original_device

end
