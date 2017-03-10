; docformat = 'rst'

;+
; Make a diagnostic plot of a CoMP raw file:
;
; :Uses:
;    comp_inventory_header, comp_constants_common, comp_config_common, comp_get_component
;
; :Params:
;    date_dir: in, required, type=string
;      the date directory corresponding to the other inputs.
;    filename: in, required, type=string
;      the name of the file in images and headers.
;    images_demod: in, required, type=array (nx by ny by n_images):
;      Array of demodulated images to plot.
;    headers_demod: in, required, type=string array (ntags by n_images):
;      Array of fits headers corresponding to images_demod.
;
; :Author:
;   Joseph Plowman
;-
pro comp_l0_diagnostic_plot, date_dir, filename, images_demod, headers_demod

	@comp_config_common

	comp_inventory_header, headers_demod, beam, wave, pol, type, expose, $
			cover, cal_pol, cal_ret

	nx = n_elements(images_demod[*,0,0])
	ny = n_elements(images_demod[0,*,0])
	pols = ['I','Q','U','V']
	npol = n_elements(pols)
	beams = [-1,1]
	diagplot_dir = filepath('l0_diagnostic_plots',subdir=comp_decompose_date(date_dir),root_dir=engineering_dir)
	if(~file_test(diagplot_dir)) then file_mkdir,diagplot_dir
	outfile = strmid(file_basename(filename),0,strpos(filename,'.',/reverse_search))+'.jpg'
	outfile_full = filepath(outfile,root_dir=diagplot_dir)

	Ip = dblarr(nx,ny)
	Qp = dblarr(nx,ny)
	Up = dblarr(nx,ny)
	Vp = dblarr(nx,ny)
	Im = dblarr(nx,ny)
	Qm = dblarr(nx,ny)
	Um = dblarr(nx,ny)
	Vm = dblarr(nx,ny)
	
	mask0=comp_raw_mask(date_dir,headers_demod,upper_left_mask=mask1, lower_right_mask=mask2)	
	if(total(pol eq 'I' and beam eq 1) gt 0) then Ip = comp_get_component(images_demod, headers_demod, 'I', 1, /noskip, /average_wavelengths)*mask0
	if(total(pol eq 'I' and beam eq -1) gt 0) then Im = comp_get_component(images_demod, headers_demod, 'I', -1, /noskip, /average_wavelengths)*mask0
	if(total(pol eq 'Q' and beam eq 1) gt 0) then Qp = comp_get_component(images_demod, headers_demod, 'Q', 1, /noskip, /average_wavelengths)*mask0
	if(total(pol eq 'Q' and beam eq -1) gt 0) then Qm = comp_get_component(images_demod, headers_demod, 'Q', -1, /noskip, /average_wavelengths)*mask0
	if(total(pol eq 'U' and beam eq 1) gt 0) then Up = comp_get_component(images_demod, headers_demod, 'U', 1, /noskip, /average_wavelengths)*mask0
	if(total(pol eq 'U' and beam eq -1) gt 0) then Um = comp_get_component(images_demod, headers_demod, 'U', -1, /noskip, /average_wavelengths)*mask0
	if(total(pol eq 'V' and beam eq 1) gt 0) then Vp = comp_get_component(images_demod, headers_demod, 'V', 1, /noskip, /average_wavelengths)*mask0
	if(total(pol eq 'V' and beam eq -1) gt 0) then Vm = comp_get_component(images_demod, headers_demod, 'V', -1, /noskip, /average_wavelengths)*mask0

	Imax = 2*median(abs(Ip[where(mask0)]))
	Qmax = 2*median(abs(Qp[where(mask0)]))
	Umax = 2*median(abs(Up[where(mask0)]))
	Vmax = 2*median(abs(Vp[where(mask0)]))

	ilabel = '; max = '+strtrim(string(Imax),2)
	qlabel = '; max = '+strtrim(string(Qmax),2)
	ulabel = '; max = '+strtrim(string(Umax),2)
	vlabel = '; max = '+strtrim(string(Vmax),2)
		
	set_plot,'z'
	device,set_resolution=[nx,ny]
	xyouts,5,5,'I +1'+ilabel,alignment=0,/device,charsize=4
	ilblimg = tvrd()
	device,/close
	xyouts,5,5,'Q +1'+qlabel,alignment=0,/device,charsize=4
	qlblimg = tvrd()
	device,/close
	xyouts,5,5,'U +1'+ulabel,alignment=0,/device,charsize=4
	ulblimg = tvrd()
	device,/close
	xyouts,5,5,'V +1'+vlabel,alignment=0,/device,charsize=4
	vlblimg = tvrd()
	device,/close
	xyouts,5,5,'I -1'+ilabel,alignment=0,/device,charsize=4
	ixlblimg = tvrd()
	device,/close
	xyouts,5,5,'Q -1'+qlabel,alignment=0,/device,charsize=4
	qxlblimg = tvrd()
	device,/close
	xyouts,5,5,'U -1'+ulabel,alignment=0,/device,charsize=4
	uxlblimg = tvrd()
	device,/close
	xyouts,5,5,'V -1'+vlabel,alignment=0,/device,charsize=4
	vxlblimg = tvrd()
	device,/close
	lblimg = [[ilblimg,qlblimg,ulblimg,vlblimg],[ixlblimg,qxlblimg,uxlblimg,vxlblimg]]

	bluimg = [[Ip/Imax,-Qp/Qmax,-Up/Umax,-Vp/Vmax],[Im/Imax,-Qm/Qmax,-Um/Umax,-Vm/Vmax]]
	grnimg = [[Ip/Imax,Qp/Qmax,Up/Umax,Vp/Vmax],[Im/Imax,Qm/Qmax,Um/Umax,Vm/Vmax]]
	redimg = bluimg
	clrimg = [[[redimg+lblimg]],[[grnimg+lblimg]],[[bluimg+lblimg]]]
	clrimg = clrimg > 0
	clrimg = 255*(clrimg < 1)

	write_jpeg,outfile_full,clrimg,quality=100,true=3
	print,'Wrote L0 diagnostic plot to '+outfile_full
	
end


