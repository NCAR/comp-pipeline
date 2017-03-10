goodfile_dir = '/hao/solar4/plowman/CoMP/engineering/2014/11/11/l0_diagnostic_plots_good/'
allfile_dir = '/hao/solar4/plowman/CoMP/engineering/2014/11/11/l0_diagnostic_plots_all/'
rawfile_dir = '/hao/solar4/plowman/CoMP/raw/20141111/'
badraw_dir = '/hao/solar4/plowman/CoMP/raw/20141111/bad_raw/'

goodfiles = file_basename(findfile(goodfile_dir+'*.FTS.jpg'),'.jpg')
allfiles = file_basename(findfile(allfile_dir+'*.FTS.jpg'),'.jpg')

ngood = n_elements(goodfiles)
nall = n_elements(allfiles)

goodflag = lonarr(nall)

for i=0,nall-1 do goodflag[i] = total(allfiles[i] eq goodfiles)

badfiles_full = rawfile_dir+allfiles(where(goodflag eq 0))
nbad = n_elements(badfiles_full)

for i=0,nbad-1 do spawn, 'mv '+badfiles_full[i]+' '+badraw_dir
