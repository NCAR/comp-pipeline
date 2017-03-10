; Attempts to convert an vector of ascii strings made from reading a fits header
; into an 'index'-type IDL structure. Its operation should be fairly generic,
; but it's only been tested on CoMP fits headers, so there are probably
; edge cases where it will fail.
function convert_ascii_header, header_in

	ntags = n_elements(header_in)
	
	; Loop through each line in the header string array
	for i=0,ntags-1 do begin
		nchar = strlen(header_in[i])
		position = 0
		varname=''
		char=''
		var = ''
		; Characters prior to an equal sign are considered part of the variable name.
		; spaces and hyphens are ignored.
		while(char ne '=' ) do begin
			char = strmid(header_in[i],position,1)
			if(char ne '=' and char ne ' ' and char ne '-') then varname+=char
			position++
			if(position ge nchar) then break
		endwhile
		if(position lt nchar) then begin
			; The next sequence of non-whitespace characters are assumed to be a variable.
			char = strmid(header_in[i],position,1)
			while(char eq ' ') do begin
				position++
				char = strmid(header_in[i],position,1)
			endwhile
			if(char eq "'") then begin
				position++
				char = strmid(header_in[i],position,1)
				while(char ne "'") do begin
					var+=char
					position++
					char = strmid(header_in[i],position,1)
				endwhile
				var = strtrim(var)
			endif else begin
				while(char ne ' ') do begin
					char = strmid(header_in[i],position,1)
					if(char ne ' ') then var+=char
					position++
				endwhile
				var = strtrim(var)
				; If the variable is numeric, then convert it to a double. will probably fail
				; for lines containing only '+','-','.', 'e' and/or 'E':
				if(strpos('+-.0123456789eE',strmid(var,0,1)) ne -1) then var = double(var)
			endelse
			; The remaining characters of the line are considered a description/comment, and stored in
			; a separate substructure:
			desc = strtrim(strmid(strmid(header_in[i],position),strpos(strmid(header_in[i],position),'/')+1))
			if(n_elements(header) eq 0) then begin
				header = create_struct(varname, var)
				descriptions = create_struct(varname, desc)
			endif else if(~tag_exist(header,varname)) then begin
				header = create_struct(header, varname, var)
				descriptions = create_struct(descriptions, varname, desc)
			endif
		endif
	endfor

	return,create_struct(header,'descriptions',descriptions)
end	
