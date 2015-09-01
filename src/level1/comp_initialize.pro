; docformat = 'rst'

;+
; Initialize constants for CoMP pipeline.
;
; Julian Date is used for selections. It can be computed from the data using::
;
;   jd = julday(month, day, year, hour, minute, second)
;
; :Uses:
;   comp_constants_common, comp_mask_constants_common
;
; :Params:
;    date_dir : in, required, type=string
;      date to process, in YYYYMMDD format
;
; :Author:
;   sitongia
;-
pro comp_initialize, date_dir
  compile_opt strictarr
  @comp_constants_common
  @comp_mask_constants_common

  debug = 0

  ; Level 1 image dimensions
  nx = 620
  ny = 620

  ; Thresholds for cutting out bad data in the L2 products
  int_thresh  = 1 ; millionths of solar disk intensity
  diff_thresh = 4 ; difference between measured and calculated line center intensity

  ; Line center wavelengths
  center1074 = 1074.62 ; was 1074.7
  center1079 = 1079.8
  center1083 = 1083.0

  stokes = ['I', 'Q', 'U', 'V']
  n_stokes = 4     ; number of stokes parameters

  ; Parse the date_dir to find the Julian date to use to switch era of constants
  year =  fix(strmid(date_dir, 0, 4))
  month = fix(strmid(date_dir, 4, 2))
  day =   fix(strmid(date_dir, 6, 2))
  jd = julday(month, day, year, 0, 0, 0)

  ; Era-specific correction factors
  ; post_rotation: offset in post position in telescope
  ;                positive shifts post clockwise in mask

  case 1 of
    ; Plate scale changed with installation of new reimaging lens on 2012-12-7
    (jd gt 2456270.5) : begin
      ; Current values
      post_rotation = 0.0        ; offset of occulter post (pixels)
      occulter_offset = 3.0      ; over or undersize occulter mask
      field_offset = -2.0        ; over or undersize field mask
      field_overlap = 16.0       ; overlap of two beams, creating "ears"
      plate_scale = 4.35         ; arcsec per pixel
    end

    ; Something changed in field, arbitrarily starting anew at 2012-6-1
    (jd gt 2456079.5) : begin
      post_rotation = 2.0
      occulter_offset = 4.0
      field_offset = -2.0
      field_overlap = 16.0
      plate_scale = 4.46
    end

    ; Changed occulter, going from existing #31 to #35, 2012-1-10
    (jd gt 2455936.5) : begin
      post_rotation = 1.0
      occulter_offset = 4.0
      field_offset = -6.0
      field_overlap = 24.0
      plate_scale = 4.46
    end

    ; Prior to occulter change on September 29, 2011
    (jd gt 2455832.5) : begin
      post_rotation = 1.0
      occulter_offset = 4.0
      field_offset = -6.0
      field_overlap = 6.0
      plate_scale = 4.46
    end

    else : begin
      ; Beginning values
      post_rotation = -2.5
      occulter_offset = 4.0
      field_offset = -6.0
      field_overlap = 6.0
      plate_scale = 4.46
    end
  endcase
end
