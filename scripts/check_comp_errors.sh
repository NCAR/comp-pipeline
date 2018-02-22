#!/bin/bash

umask 0002
output="/tmp/CoMP-$$"
if [[ $# -lt 1 ]]; then
  date="yesterday"
else
  date=$1
fi
yesterday=$(date +"%Y%m%d" --date="$date")
log_name=/hao/acos/comp/logs/$yesterday.comp.log
raw_dir=/hao/mahidata1/Data/CoMP/raw/$yesterday

if [ -f $log_name ]; then
  # check total running time
  grep "INFO: COMP_RUN_PIPELINE: total running time:" $log_name >> $output 2>&1
  if [ $? -eq 1 ]; then
    echo -e "CoMP pipeline not finished yet at $(date +'%Y-%m-%d %H:%M:%S')\n" >> $output 2>&1
  fi

  # check for raw data files
  echo -e "\n# Raw files in $raw_dir" >> $output 2>&1
  nraw=$(find $raw_dir -name '*.FTS' | wc -l)
  echo -e "\n$nraw raw files\n" >> $output 2>&1

  # check yesterday's CoMP log file for errors
  pattern="(WARN|ERROR|CRITICAL):"
  echo -e "\n# CoMP pipeline error messages from $log_name\n" >> $output 2>&1
  grep -E $pattern $log_name >> $output 2>&1

  # check for HPSS errors
  gateway_logdir=/hao/acos/sw/var/log/CoMP/cidx
  hpss_log_name=${gateway_logdir}/HPSSGateway-CoMP-$(date +%F --date="$date").log
  echo -e "\n# HPSS error messages from $hpss_log_name\n" >> $output 2>&1
  grep FAILED $hpss_log_name >> $output 2>&1
else
  echo "Log file $log_name does not exist" >> $output 2>&1
fi

# indicate who sent this email
echo -e "\n\nSent by $(readlink -f $0) ($(whoami)@$(hostname))" >> $output 2>&1

# email results

# TODO: change recipients when pushed to production
#recipient="iguana@ucar.edu, detoma@ucar.edu, mgalloy@ucar.edu, berkey@ucar.edu"
recipient="mgalloy@ucar.edu"

mail -s "CoMP messages from $yesterday logs" -r $(whoami)@ucar.edu "$recipient" < $output

# clean up
rm $output

