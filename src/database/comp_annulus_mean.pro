; docformat = 'rst'

;+
; Find means of polar coordinate grid for a single annulus at a given solar
; radii.
;
; :Returns:
;   float
;
; :Params:
;   image : in, required, type="fltarr(n, n)"
;     image to find the annular grid means from
;   inner_radius : in, required, type=float
;     inner boundary of annulus [rsun]
;   outer_radius : in, required, type=float
;     outer boundary of annulus [rsun]
;   sun_pixels : in, required, type=float
;     number of pixels corresponding to a solar radius
;-
function comp_annulus_mean, image, inner_radius, outer_radius, sun_pixels, $
                            median=median
  compile_opt strictarr

  dims = size(image, /dimensions)
  n = dims[0]

  d = mg_dist(n, /center)

  annulus_indices = where((d gt (sun_pixels * inner_radius)) $
                            and (d lt (sun_pixels * outer_radius)), $
                          n_annulus)


  return, n_annulus eq 0L $
            ? !null $
            : (keyword_set(median) $
              ? median(image[annulus_indices]) $
              : mean(image[annulus_indices], /nan))
end
