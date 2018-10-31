root_dir = '/hao/mahidata1/Data/CoMP'
eng_dir1 = filepath('engineering.centering/2016/07/07', root=root_dir)
eng_dir2 = filepath('engineering.centering-v1.2.4/2016/07/07', root=root_dir)

intermediate_filename = '20160707.071102.comp.1074.iqu.5.flatcor.fts'

fits_open, filepath(intermediate_filename, root=eng_dir1), fcb1
fits_open, filepath(intermediate_filename, root=eng_dir2), fcb2

fits_read, fcb1, primary_data1, primary_header1, exten_no=0
normalize = sxpar(primary_header1, 'NORMALIZ')
fits_read, fcb1, data1, header1, exten_no=5 

raw_exts = long(strtrim(strsplit(sxpar(header1, 'RAWEXT'), ',', /extract), 2))
print, n_elements(raw_exts), format='(%"%d raw extensions")'
fits_read, fcb2, data21, header21, exten_no=raw_exts[0]
fits_read, fcb2, data22, header22, exten_no=raw_exts[1]

flat1_filename = 'process.centering/20160707/level1/20160707.comp.flat.fts'
flat2_filename = 'process.centering-v1.2.4/20160707/level1/flat.fts'

fits_open, flat1_filename, flat_fcb1
fits_open, flat2_filename, flat_fcb2

fits_read, flat_fcb1, flat1, flat_header1, exten_no=sxpar(header1, 'FLATEXT')
flat_ext21 = sxpar(header21, 'FLATEXT')
flat_ext22 = sxpar(header22, 'FLATEXT')
if (flat_ext21 ne flat_ext22) then message, 'FLATEXTs do not match'
fits_read, flat_fcb2, flat2, flat_header2, exten_no=flat_ext21

data2 = data21 + data22
data2 = 0.5 * data2 * 84.0 / normalize

comp_fix_stray_light, data1, flat_header1, fit1, coefficients=kx1
comp_fix_stray_light, data22, flat_header2, fit22, coefficients=kx22
comp_fix_stray_light, data21, flat_header2, fit21, coefficients=kx21
