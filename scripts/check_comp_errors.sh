#!/bin/bash

umask 0002
output="/tmp/CoMP-$$"

log_name=/hao/acos/comp/logs/$(date +"%Y%m%d" --date="yesterday").log

# check total running time
grep "INFO: COMP_RUN_PIPELINE: Total running time:" $log_name >> $output 2>&1
if [ $? -eq 1 ]; then
  echo "CoMP pipeline not finished yet at $(date +'%Y-%m-%d %H:%M:%S')" >> $output 2>&1
fi

# check yesterday's CoMP log file for errors
pattern="(WARN|ERROR|CRITICAL):"
echo -e "\n# CoMP pipeline error messages from $log_name\n" >> $output 2>&1
grep -E $pattern $log_name >> $output 2>&1

# check for HPSS errors
gateway_logdir=/hao/acos/sw/var/log/CoMP/cidx
hpss_log_name=${gateway_logdir}/HPSSGateway-CoMP-$(date +%F --date="yesterday").log
echo -e "\n# HPSS error messages from $hpss_log_name\n" >> $output 2>&1
grep FAILED $hpss_log_name >> $output 2>&1

# indicate who sent this email
echo -e "\nSent by $0" >> $output 2>&1

# email results
recipient="iguana@ucar.edu, detoma@ucar.edu, mgalloy@ucar.edu, berkey@ucar.edu"
mail -s "CoMP messages from $yesterday logs" "$recipient" < $output

# clean up
rm $output

