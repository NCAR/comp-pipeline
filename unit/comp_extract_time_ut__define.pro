; docformat = 'rst'

pro comp_extract_time_ut::_check, headers, $
                                  day=sday, month=smonth, year=syear, $
                                  hours=shours, mins=smins, secs=ssecs
  compile_opt strictarr

  time = comp_extract_time(headers, day, month, year, hours, mins, secs)

  assert, day eq sday, 'incorrect day %d instead of %d', day, sday
  assert, month eq smonth, 'incorrect month %d instead of %d', month, smonth
  assert, year eq syear, 'incorrect year %d instead of %d', year, syear
  assert, hours eq shours, 'incorrect hours %d instead of %d', hours, shours
  assert, mins eq smins, 'incorrect mins %d instead of %d', mins, smins
  assert, secs eq ssecs, 'incorrect secs %d instead of %d', secs, ssecs

  standard_time = shours + smins / 60.0 + ssecs / 60.0 / 60.0
  assert, time eq standard_time, 'incorrect time: %f', time
end


function comp_extract_time_ut::test_am
  compile_opt strictarr

  header = ['DATE_OBS=          ''2/26/2015'' / DATE OF OBSERVATION', $
            'TIME_OBS=         ''7:53:10 AM'' / TIME OF OBSERVATION']

  self->_check, header, day=26, month=2, year=2015, hours=7, mins=53, secs=10

  return, 1
end


function comp_extract_time_ut::test_pm
  compile_opt strictarr

  header = ['DATE_OBS=          ''2/26/2015'' / DATE OF OBSERVATION', $
            'TIME_OBS=         ''7:53:10 PM'' / TIME OF OBSERVATION']

  self->_check, header, day=26, month=2, year=2015, hours=19, mins=53, secs=10

  return, 1
end


function comp_extract_time_ut::test_noon
  compile_opt strictarr

  header = ['DATE_OBS=          ''2/26/2015'' / DATE OF OBSERVATION', $
            'TIME_OBS=         ''12:00:00 PM'' / TIME OF OBSERVATION']

  self->_check, header, day=26, month=2, year=2015, hours=12, mins=0, secs=0

  return, 1
end


function comp_extract_time_ut::test_midnight
  compile_opt strictarr

  header = ['DATE_OBS=          ''2/26/2015'' / DATE OF OBSERVATION', $
            'TIME_OBS=         ''12:00:00 AM'' / TIME OF OBSERVATION']

  self->_check, header, day=26, month=2, year=2015, hours=0, mins=0, secs=0

  return, 1
end


function comp_extract_time_ut::init, _extra=e
  compile_opt strictarr

  if (~self->CoMPutTestCase::init(_extra=e)) then return, 0

  self->addTestingRoutine, ['comp_extract_time', 'comp_parse_time'], /is_function

  return, 1
end


pro comp_extract_time_ut__define
  compile_opt strictarr

  define = { comp_extract_time_ut, inherits CoMPutTestCase }
end