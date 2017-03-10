function guess_background_filename, fg_filename, fgvar_filename=fgvar_filename, bgvar_filename=bgvar_filename

	fgfile_base = file_basename(fg_filename)
	fgfile_dir = file_dirname(fg_filename)
	
	fgfile_split = strsplit(fg_filename,'.',/extract)
	fgvarfile_split = fgfile_split
	bgvarfile_split = fgfile_split
	
	ftsindices = where(fgfile_split eq 'fts' or fgfile_split eq 'fits',count)
	if(count gt 0) then begin
		fgvarfile_split[ftsindices] = 'var.'+fgfile_split[ftsindices]
		bgvarfile_split[ftsindices] = 'bkg.var.'+fgfile_split[ftsindices]
		fgfile_split[ftsindices] = 'bkg.'+fgfile_split[ftsindices]
		
		bgfile_guess = strjoin(fgfile_split,'.')
		fgvar_filename = strjoin(fgvarfile_split,'.')
		bgvar_filename = strjoin(bgvarfile_split,'.')
	endif
	
	return,bgfile_guess
	
end

