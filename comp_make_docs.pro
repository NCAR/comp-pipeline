; docformat = 'rst'

;+
; Use IDLdoc to create API documentation for CoMP pipeline.
;
; :Keywords:
;   user : in, optional, type=boolean
;     set to generate user-level documentation
;-
pro comp_make_docs, user=user
  compile_opt strictarr

  root = mg_src_root()
  idldoc, root=filepath('src', root=root), $
          output=filepath(keyword_set(user) $
                            ? 'api-userdocs' $
                            : 'api-docs', $
                          root=root), $
          title='CoMP pipeline API documentation', $
          subtitle='NCAR-HAO MLSO', $
          /embed, /statistics, /use_latex, $
          format_style='rst', $
          overview=filepath('overview.txt', root=root), $
          user=user
end
