; docformat = 'rst'

;= access configuration values

;+
; Retrieve a configuration value.
;
; :Returns:
;   type dependent on section/option
;
; :Params:
;   section : in, required, type=string
;     section name of the configuration file
;   option : in, required, type=string
;     option name in the given section of the configuration file
;-
function comp_configuration::get, section, option
  compile_opt strictarr

  extract = 0B

  case section of
    'input': begin
        case option of
          'date_pattern': default = '*'
          else:
        endcase
      end
    'processing': begin
        case option of
          'wavelengths': begin
              default = ['1074', '1079', '1083']
              extract = 1B
            end
          else:
        endcase
      end
    'logging': begin
        case option of
          'level': default = 4L
          else:
        endcase
      end
    'actions': begin
        case option of
          'mail_warnings': default = 0B
          'send_to_hpss': default = 0B
          else:
        endcase
      end
    else:
  endcase

  return, self.config->get(option, section=section, $
                           default=default, $
                           extract=extract)
end


;= operator overloading

;+
; Returns representation of the configuration.
;
; :Returns:
;   strarr
;-
function comp_configuration::_overloadPrint
  compile_opt strictarr

  return, self.congif::_overloadPrint()
end


;= lifecycle methods

;+
; Create a configuration object.
;
; :Returns:
;   1 for success, 0 for failure
;
; :Keywords:
;   filename : in, required, type=string
;     configuration filename
;-
function comp_configuration::init, filename=filename
  compile_opt strictarr

  self.config = mg_read_config(filename)

  return, 1
end


;+
; Define configuration class.
;-
pro comp_configuration__define
  compile_opt strictarr

  define = { comp_configuration, inherits IDL_Object, $
             config: obj_new() $
           }
end