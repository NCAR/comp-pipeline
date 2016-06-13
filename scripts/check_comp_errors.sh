#!/bin/bash

umask 0002
output="/tmp/CoMP-$$"

# check yesterday's CoMP log file for errors
log_name=/hao/acos/comp/logs/$(date +"%Y%m%d" --date="yesterday").log
pattern="(WARN|ERROR|CRITICAL):"
echo -e "# CoMP pipeline error messages from $log_name\n" >> $output 2>&1
grep -E $pattern $log_name >> $output 2>&1

# check for HPSS errors
gateway_logdir=/hao/acos/sw/var/log/CoMP/cidx
hpss_log_name=${gateway_logdir}/HPSSGateway-CoMP-$(date +%F --date="yesterday").log
echo -e "\n# HPSS error messages from $hpss_log_name\n" >> $output 2>&1
grep FAILED $hpss_log_name >> $output 2>&1

# email results
recipient="iguana@ucar.edu, detoma@ucar.edu, mgalloy@ucar.edu"
mail -s "CoMP messages from $yesterday logs" "$recipient" < $output

# clean up
rm $output

