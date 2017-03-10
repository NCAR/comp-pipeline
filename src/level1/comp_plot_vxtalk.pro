; docformat = 'rst'

;+
; Plot the stokes V crosstalk fit obtained by comp_find_vxtalk.pro.
;
; :Uses:
;    None
;
; :Params:
;    I : in, required, type=array (nx by ny)
;      Stokes I used for crosstalk fitting
;    Q : in, required, type=array (nx by ny) 
;      Stokes I used for crosstalk fitting
;    U : in, required, type=array (nx by ny) 
;      Stokes I used for crosstalk fitting
;    V : in, required, type=array (nx by ny) 
;      Stokes I used for crosstalk fitting
;    IVx : in, required, type=array (nx by ny)
;      I to V crosstalk fit
;    QVx : in, required, type=array (nx by ny) 
;      Q to V crosstalk fit
;    UVx : in, required, type=array (nx by ny) 
;      U to V crosstalk fit
;    mask : in, required, type=array (nx by ny)
;      Mask to remove Occulted areas from fit
;    filename : in, required, type=string
;      Label for the file sequence. Ignored after first call unless reset_counter is set.
;    ilabel : in, required, type=string
;      Additional label to add to Stokes I plots (typically the Stokes I filename)
;    qlabel : in, required, type=string
;      Additional label to add to Stokes Q plots (typically the Stokes Q filename)
;    ulabel : in, required, type=string
;      Additional label to add to Stokes U plots (typically the Stokes U filename)
;    vlabel : in, required, type=string
;      Additional label to add to Stokes V plots (typically the Stokes V filename)
;    reset_counter : in, required, type=keyword
;      Successive calls in a sequence will have filenames indexed by a counter, and otherwise
;      they are identical. This resets the counter and uses the new filename handed in.
;
; :Author:
;   Joseph Plowman
;-
pro comp_plot_vxtalk, I, Q, U, V, IVx, QVx, UVx, mask, filename, ilabel, qlabel, ulabel, vlabel,reset_counter=reset_counter

	nx = n_elements(I[*,0])
	ny = n_elements(I[0,*])
	common plot_v_crosstalk_block, counter, filename_prefix
	
	; Check to see if we need to reset the filename/counter and then make the filename:
	if(n_elements(filename_prefix) eq 0 or keyword_set(reset_counter)) then begin
		counter=0
		filename_prefix = filename
	endif else begin
		counter++
	endelse
	outfile = filename_prefix + string(format='(%"%04d.jpg")',counter)
	
	; Compute the maximum levels for each value:
	Imax = 2*median(abs(I[where(mask)]))
	Qmax = 2*median(abs(Q[where(mask)]))
	Umax = 2*median(abs(U[where(mask)]))
	Vmax = 2*median(abs(V[where(mask)]))
	Ix = IVx*(mask)
	Qx = QVx*(mask)
	Ux = UVx*(mask)
	Ixmax = max(abs(Ix))
	Qxmax = max(abs(Qx))
	Uxmax = max(abs(Ux))
	Vfit = I*IVx+Q*QVx+U*UVx
	Vresid = (Vfit-V)*mask
	print,max(V/Vmax)
	
	; Make the text overlay images for each subimages:
	charsz = 2.5*nx/1024
	set_plot,'z'
	device,set_resolution=[nx,ny]
	xyouts,5,5,'I '+ilabel+' max= '+strtrim(string(Imax),2),alignment=0,/device,charsize=charsz,charthick=2
	ilblimg = tvrd()
	device,/close
	xyouts,5,5,'Q '+qlabel+' max= '+strtrim(string(Qmax),2),alignment=0,/device,charsize=charsz,charthick=2
	qlblimg = tvrd()
	device,/close
	xyouts,5,5,'U '+ulabel+' max= '+strtrim(string(Umax),2),alignment=0,/device,charsize=charsz,charthick=2
	ulblimg = tvrd()
	device,/close
	xyouts,5,5,'V '+vlabel+' max= '+strtrim(string(Vmax),2),alignment=0,/device,charsize=charsz,charthick=2
	vlblimg = tvrd()
	device,/close
	xyouts,5,5,'I-to-V coefficient '+ilabel+' max= '+strtrim(string(Ixmax),2),alignment=0,/device,charsize=charsz,charthick=2
	ixlblimg = tvrd()
	device,/close
	xyouts,5,5,'Q-to-V coefficient '+qlabel+' max= '+strtrim(string(Qxmax),2),alignment=0,/device,charsize=charsz,charthick=2
	qxlblimg = tvrd()
	device,/close
	xyouts,5,5,'U-to-V coefficient '+ulabel+' max= '+strtrim(string(Uxmax),2),alignment=0,/device,charsize=charsz,charthick=2
	uxlblimg = tvrd()
	device,/close
	xyouts,5,5,'V residual '+vlabel+' max= '+strtrim(string(Vmax),2),alignment=0,/device,charsize=charsz,charthick=2
	vxlblimg = tvrd()
	device,/close
	; Assemble the overlays:
	lblimg = [[ilblimg,qlblimg,ulblimg,vlblimg],[ixlblimg,uxlblimg,qxlblimg,vxlblimg]]

	; Assemble the subimages and add the overlay:
	bluimg = [[I*mask/Imax,-Q*mask/Qmax,-U*mask/Umax,-V*mask/Vmax],[-Ix/Ixmax,-Qx/Qxmax,-Ux/Uxmax,-Vresid/Vmax]]
	grnimg = [[I*mask/Imax,Q*mask/Qmax,U*mask/Umax,V*mask/Vmax],[Ix/Ixmax,Qx/Qxmax,Ux/Uxmax,Vresid/Vmax]]
	redimg = bluimg
	clrimg = [[[redimg+lblimg]],[[grnimg+lblimg]],[[bluimg+lblimg]]]
	clrimg = clrimg > 0
	clrimg = 255*(clrimg < 1)

	; Write to jpeg:
	write_jpeg,outfile,clrimg,quality=100,true=3
	print,'Wrote Stokes V crosstalk plot to '+outfile
	
end

