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

L1_FILES=$(ls | egrep ".{15}\.comp\.${WAVE}\.[iquv]{1,4}\.(3|5|11)\.fts.gz")
L1_BKG_FILES=$(ls | egrep ".{15}\.comp\.${WAVE}\.[iquv]{1,4}\.(3|5|11)\.bkg\.fts.gz")
L1_INTENSITY_FILES=$(ls | egrep ".{15}\.comp\.${WAVE}\.intensity\.fts.gz")
OTHER_FILES="*.comp.cal.files.txt *.comp.dark.files.txt *.comp.opal.files.txt *.comp.flat.fts *.comp.dark.fts"
WAVE_FILES="*.comp.${WAVE}.files.txt *.comp.${WAVE}.good.iqu.files.txt *.comp.${WAVE}.good.all.files.txt *.comp.${WAVE}.good.waves.files.txt *.comp.${WAVE}.good.synoptic.files.txt *.comp.${WAVE}.gbu.log"

L1_TARNAME=${DATE}.comp.${WAVE}.l1.tgz 
cmd="tar czf ${L1_TARNAME} ${L1_FILES} ${L1_BKG_FILES} ${L1_INTENSITY_FILES} ${OTHER_FILES} ${WAVE_FILES}"
$cmd

if [ -h ${HPSS_GATEWAY}/${L1_TARNAME} ]; then
  rm ${HPSS_GATEWAY}/${L1_TARNAME}
fi

ln -s ${DIR}/${L1_TARNAME} ${HPSS_GATEWAY}
