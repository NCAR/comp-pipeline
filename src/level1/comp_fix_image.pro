; docformat = 'rst'

;+
; Function to fix CoMP columns. Moves bad columns to the left edge of
; the image.
;
; :Params:
;   data : in, required, type=fltarr
;     1024 by 1024 raw image
;
; :Keywords:
;   old_data : in, optional, type=boolean
;     set to use the old pattern
;-
function comp_fix_image, data, old_data=old_data
  compile_opt strictarr

  data2 = data

  if (keyword_set(old_data)) then begin
    ; put bad columns at left edge of image
    data2[0, 512:1023] = data[0,   512:1023]
    data2[1, 512:1023] = data[256, 512:1023]
    data2[2, 512:1023] = data[512, 512:1023]
    data2[3, 512:1023] = data[768, 512:1023]

    data2[0, 0:511] = data[255,  0:511]
    data2[0, 0:511] = data[511,  0:511]
    data2[0, 0:511] = data[767,  0:511]
    data2[0, 0:511] = data[1023, 0:511]

    ; squeeze image back
    data2[4:258,    512:1023] = data[1:255,    512:1023]
    data2[259:513,  512:1023] = data[257:511,  512:1023]
    data2[514:768,  512:1023] = data[513:767,  512:1023]
    data2[769:1023, 512:1023] = data[769:1023, 512:1023]

    ; squeeze image back
    data2[4:258,    0:511] = data[0:254,    0:511]
    data2[259:513,  0:511] = data[256:510,  0:511]
    data2[514:768,  0:511] = data[512:766,  0:511]
    data2[769:1023, 0:511] = data[768:1022, 0:511]

    data2 = reverse(data2, 2, /overwrite)
  endif else begin
    ; move columns 0, 256, 512, and 768 to columns 0..3

    ; put bad columns at left edge of image
    data2[0, 0:1023] = data[0,   0:1023]
    data2[1, 0:1023] = data[256, 0:1023]
    data2[2, 0:1023] = data[512, 0:1023]
    data2[3, 0:1023] = data[768, 0:1023]

    ; squeeze image back
    data2[4:258,    0:1023] = data[1:255,    0:1023]
    data2[259:513,  0:1023] = data[257:511,  0:1023]
    data2[514:768,  0:1023] = data[513:767,  0:1023]
    data2[769:1023, 0:1023] = data[769:1023, 0:1023]
  endelse

 return, data2
end
