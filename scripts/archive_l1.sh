#!/bin/sh

# syntax:
#
#  archive_l1.sh date wave hpss_gateway delay_time
#
# For example:
#
#   archive_l1.sh 20150801 1074 /export/data1/Data/CoMP/test-hpss.l2test 2h

DATE=$1
WAVE=$2
HPSS_GATEWAY=$3
DELAY_TIME=$4
DIR=${PWD}

sleep ${DELAY_TIME}

L1_FILES=$(ls | egrep ".{15}\.comp\.${WAVE}\.[iquv]{1,4}\.(3|5|11)\.fts")
L1_BKG_FILES=$(ls | egrep ".{15}\.comp\.${WAVE}\.[iquv]{1,4}\.(3|5|11)\.bkg\.fts")
OTHER_FILES="cal_files.txt dark_files.txt opal_files.txt flat.fts dark.fts"
WAVE_FILES="${WAVE}_files.txt good_${WAVE}_files.txt good_all_${WAVE}_files.txt good_waves_${WAVE}_files.txt synoptic_${WAVE}_files.txt GBU.${WAVE}.log"

L1_TARNAME=${DATE}.comp.${WAVE}.l1.tgz 
cmd="tar czf ${L1_TARNAME} ${L1_FILES} ${L1_BKG_FILES} ${OTHER_FILES} ${WAVE_FILES}"
$cmd

if [ -h ${HPSS_GATEWAY}/${L1_TARNAME} ]; then
  rm ${HPSS_GATEWAY}/${L1_TARNAME}
fi

ln -s ${DIR}/${L1_TARNAME} ${HPSS_GATEWAY}