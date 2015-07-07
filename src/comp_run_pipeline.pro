; docformat = 'rst'

;+
; Run the pipeline.
;
; :Params:
;   configuration_filename : in, required, type=string
;     filename of configuration file specifying the parameters of the run
;-
pro comp_run_pipeline, configuration_filename
  compile_opt strictarr

  config = comp_configuration(filename=configuration_filename)

  ; level 1 processing
  ; level 2 processing
end
