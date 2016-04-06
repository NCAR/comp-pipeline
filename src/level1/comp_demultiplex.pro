; docformat = 'rst'

;+
; Function to demultiplex comp data from four tap interleaved output into
; correct image order.
;
; :Returns:
;   `intarr`
;
; :Params:
;   data
;-
function comp_demultiplex, data
  compile_opt strictarr

  num = n_elements(data)

  d = reform(data, 8, num / 8)
  d = transpose(d)
  d = reform(d, 256, 512, 8)

  remap = [5, 4, 7, 6, 0, 1, 3, 2]
  dd = intarr(1024, 1024)
  dd[0:255, 0:511] = d[*, *,remap[0]]
  dd[256:511, 0:511] = d[*, *, remap[1]]
  dd[512:767, 0:511] = d[*, *, remap[2]]
  dd[768:1023, 0:511] = d[*, *, remap[3]]
  dd[0:255, 512:1023] = reverse(d[*, *,remap[4]], 2)
  dd[256:511, 512:1023] = reverse(d[*, *,remap[5]], 2)
  dd[512:767, 512:1023] = reverse(d[*, *,remap[6]], 2)
  dd[768:1023, 512:1023] = reverse(d[*, *, remap[7]], 2)

  return, dd
end