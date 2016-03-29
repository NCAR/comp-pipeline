; docformat = 'rst'

beam = -1
wave = 1075
exact_wave = 1074.62
locations = [[483, 542], [109, 902]]

cal_directory = '/export/data1/Data/CoMP/raw.calibration/20150729/'
config_filename = 'config/comp.mgalloy.compdata.calibration.cfg'

date_dir = file_basename(cal_directory)
comp_configuration, config_filename=config_filename
comp_initialize, date_dir

@comp_config_common

; standard CoMP images are 1k by 1k, before beam combining                                 
if (n_elements(nx) eq 0) then nx = 1024
if (n_elements(ny) eq 0) then ny = 1024

; find the calibration files in the cal directory                                          
files = file_search(cal_directory + '*.FTS')
cal_info = comp_get_cal_info(files)

; the files which have the correct line ('wave'), are not darks ('cvers') and              
; are labeled as cal data                                                                  
calfiles = where(cal_info.waves eq wave and cal_info.cvers and cal_info.cpols)
nfiles = n_elements(calfiles)   ; total number of cal files.   

; the number polarization analyzer states in each file                                     
n_mpols = total(cal_info.mpols ne '', 2)
  
; find the unique measured polarizations                                                   
upols = cal_info.mpols[uniq(cal_info.mpols, sort(cal_info.mpols))]
upols = upols[where(upols ne '')]
n_upols = n_elements(upols)

; find the unique cal optics configurations                                                
ucals = cal_info.ctags[uniq(cal_info.ctags, sort(cal_info.ctags))]
n_ucals = n_elements(ucals)

; total number of images needed for all unique combinations of polarization                
; analyzer and calibration optics configurations                                           
ndata = total(n_upols * n_ucals)

; arrays which will label the polarization analyzer and calibration optics                 
; states for each image in the data array                                                  
datapols = strarr(ndata)
for i = 0, n_upols - 1 do begin
  for j = 0, n_ucals - 1 do begin
    datapols[i * n_ucals + j] = upols[i]
  endfor
endfor

angles = ['-1', '44', '89', '134']
retarder = ['0', '1']
datacals = strarr(ndata)
for i = 0L, 5L do begin
  for r = 0L, n_elements(retarder) - 1L do begin
    for a = 0L, n_elements(angles) - 1L do begin
      index = i * n_elements(angles) * n_elements(retarder) + r * n_elements(angles) + a
      datacals[index] = string(retarder[r], angles[a], format='(%"1 %s %s")')
    endfor
  endfor
endfor

n_images = lonarr(ndata)

for i = 0L, nfiles - 1L do begin
  ii = calfiles[i]
  comp_read_data, cal_info.files[ii], images, headers, header0
  cal = cal_info.ctags[ii] ; cal optics state for this file (assumes only one per file)

  ; loop over the measured polarizations in the file                                       
  for j = 0, n_mpols[ii] - 1 do begin
    pol = cal_info.mpols[ii, j]   ; the current measured polarization

    idata = where(datapols eq pol and datacals eq cal)

    comp_inventory_header, headers, beams, groups, waves, polstates, type, $
                           expose, cover, cal_pol, cal_ret

    beams[0] = 2  ; never match first image in a file                                      
    !null = where(waves eq exact_wave and polstates eq pol and beams eq beam, $
                  n_matching_waves)
    n_images[idata] += n_matching_waves
  endfor
endfor

; all used images before median                                                            
max_images = max(n_images)
raw_data = fltarr(nx, ny, max_images, ndata)
raw_flat_data = fltarr(nx, ny, max_images, ndata)
dark_data = fltarr(nx, ny, ndata)

n_found_images = lonarr(ndata)

for i = 0L, nfiles - 1L do begin
  ii = calfiles[i]
  comp_read_data, cal_info.files[ii], images, headers, header0
  comp_apply_flats_darks, images, headers, date_dir, flat_header=flat_header, $
                          flat_images=flat_images, dark_image=dark_image, $
                          /no_apply

  cal = cal_info.ctags[ii] ; cal optics state for this file (assumes only one per file)

  ; loop over the measured polarizations in the file                                       
  for j = 0, n_mpols[ii] - 1 do begin
    pol = cal_info.mpols[ii, j]   ; the current measured polarization

    idata = where(datapols eq pol and datacals eq cal)

    comp_inventory_header, headers, beams, groups, waves, polstates, type, $
                           expose, cover, cal_pol, cal_ret

    beams[0] = 2  ; never match first image in a file                                      
    ind = where(waves eq exact_wave and polstates eq pol and beams eq beam, $
                  n_matching_waves)

    for w = 0L, n_matching_waves - 1L do begin
      raw_data[*, *, n_found_images[idata], idata] = images[*, *, ind[w]]
      raw_flat_data[*, *, n_found_images[idata], idata] = flat_images[*, *, ind[w]]
      dark_data[*, *, idata] = dark_image
      n_found_images[idata]++
    endfor
  endfor
endfor

for i = 0L, n_elements(locations) / 2 - 1L do begin
  location = locations[*, i]
  data = fltarr(nx, ny, ndata)

  for d = 0L, ndata - 1L do begin
    data[*, *, d] = median(raw_data[*, *, 0:n_images[d] - 1, d], dimension=3)
  endfor

  loc_mask = bytarr(nx, ny)
  loc_mask[location[0] - 2:location[0] + 2, location[1] - 2: location[1] + 2] = 1B
  
  mask_ind = where(loc_mask, n_values)

  ; write clear.out
  dark_values = dblarr(ndata, n_values)
  for j = 0L, ndata - 1L do begin
    darkj = dark_data[*, *, j]
    dark_values[j, *] = darkj[mask_ind]
  endfor

  flat_data = fltarr(nx, ny, ndata)
  for d = 0L, ndata - 1L do begin
    flat_data[*, *, d] = median(raw_flat_data[*, *, 0:n_images[d] - 1, d], dimension=3)
  endfor
  flat_data = reform(flat_data, nx, ny, 6, 8)
  flat_data = median(flat_data, dimension=4)

  flat_values = dblarr(6, n_values)
  for s = 0L, 6L - 1L do begin
    flats = flat_data[*, *, s]
    flat_values[s, *] = flats[mask_ind]
  endfor

  openw, lun, string(i + 1, format='(%"clear-%d.out")'), /get_lun, width=100
  printf, lun, mean(dark_values), min(dark_values), max(dark_values)
  printf, lun
  printf, lun, flat_values
  free_lun, lun

  ; write cal.out
  data_values = dblarr(ndata, n_values)
  for d = 0L, ndata - 1L do begin
    datad = data[*, *, d]
    data_values[d, *] = datad[mask_ind]
  endfor

  openw, lun, string(i + 1, format='(%"cal-%d.out")'), /get_lun

  angles = [0.0, 45.0, 90.0, 135.0]
  for r = 0, 1 do begin
    printf, lun, transpose([[angles], [fltarr(4)]]), format='(2F10.2)'
  endfor
  printf, lun
  for p = 0L, n_values - 1L do begin
    printf, lun, reform(data_values[*, p], 8, 6), format='(8F10.2)'
  endfor
  free_lun, lun
endfor

end
