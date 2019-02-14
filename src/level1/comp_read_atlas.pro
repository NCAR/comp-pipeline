; docformat = 'rst'

;+
; Procedure to read the atlas data of ``Solar Flux Atlas from 296 to
; 1300 nm'' by Robert L. Kurucz, Ingemar Furenlid, James Brault, and
; Larry Testerman, (National Solar Observatory Atlas No. 1, June 1984).
;    
; :Examples:
;    For example::
;
;      data = comp_read_atlas(5890.0, 5.0)
;      plot, data[0, *], data[1, *]   ; will plot 5A around the Na D2 line
;
; :Returns:
;    `fltarr(3, n)`. Returns a two dimensional array with the first
;    dimension containing the wavelength and the second dimension
;    containing the flux normalized to the continuum. Returns -1 on
;    error.
;
; :Params:
;   wave : in, required, type=float
;     central wavelength in Angstroms
;   range : in, required, type=float
;     wavelength range in Angstroms
;
; :Author:
;   MLSO Software Team
;-
function comp_read_atlas, wave, range
  compile_opt strictarr
  @comp_config_common

  lw = 4 * fix(fix(wave / 10.0) / 4.0)

  basename = string(lw, format='("lm",i4.4)')
  mg_log, basename, /debug, name='comp'

  filename = filepath(basename, $
                      subdir=['..', 'resource', 'atlas'], $
                      root=binary_dir)

  openr, lun, filename, /get_lun
  stat = fstat(lun)
  nwave = stat.size / 38

  data = fltarr(3, nwave)
  readf, lun, data
  free_lun, lun

  data[0, *] = data[0, *] * 10.0   ; convert to Angstroms
  good = where(data[0, *] lt wave + range / 2.0 $
                 and data[0, *] gt wave - range / 2.0, count)
  if (count gt 0L) then return, data[0:2, good] else return, -1L
end
