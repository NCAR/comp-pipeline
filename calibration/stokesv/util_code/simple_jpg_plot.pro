pro simple_jpg_plot,filename,image_in,min=min,max=max,true=true
	
	if(n_elements(min) eq 0) then min = min(image_in)
	if(n_elements(max) eq 0) then max = max(image_in)
	
	image = (((image_in < max) - min) > 0)*255.0/(max-min)
	write_jpeg,filename,image,quality=100,true=true
	
end