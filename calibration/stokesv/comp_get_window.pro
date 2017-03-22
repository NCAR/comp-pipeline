function comp_get_window_exist, index
  compile_opt strictarr

  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    return, 0B
  endif

  wset, index

  return, 1B
end

pro comp_get_window, index, _extra=e
  compile_opt strictarr

  if (~comp_get_window_exist(index)) then window, index, _extra=e
end
