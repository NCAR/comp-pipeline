#!/bin/sh

# syntax:
#
#  archive_l2.sh date wave hpss_gateway delay_time
#
# For example:
#
#   archive_l2.sh 20150801 1074 /export/data1/Data/CoMP/test-hpss.l2test 2h

DATE=$1
WAVE=$2
HPSS_GATEWAY=$3
DELAY_TIME=$4
DIR=${PWD}

sleep ${DELAY_TIME}

DAILY_FILES=$(ls | egrep "${DATE}\.comp\.${WAVE}\.(mean|median|sigma|quick_invert)\.fts")
POLARIZATION_FILES="*.polarization.fts.gz"
DYNAMICS_FILES="*.dynamics.fts.gz"
L2_FILES="${DAILY_FILES} ${POLARIZATION_FILES} ${DYNAMICS_FILES}"

L2_TARNAME=${DATE}.comp.${WAVE}.l2.tgz 
cmd="tar czf ${L2_TARNAME} ${L2_FILES}"
$cmd

if [ -h ${HPSS_GATEWAY}/${L2_TARNAME} ]; then
  rm ${HPSS_GATEWAY}/${L2_TARNAME}
fi

ln -s ${DIR}/${L2_TARNAME} ${HPSS_GATEWAY}
