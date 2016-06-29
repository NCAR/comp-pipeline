#!/bin/sh

# syntax:
#
#   archive_l0.sh date hpss_gateway delay_time
#
# For example:
#
#   archive_l0.sh 20150801 /export/data1/Data/CoMP/test-hpss.l2test 12h

DATE=$1
HPSS_GATEWAY=$2
DELAY_TIME=$3
DIR=${PWD}

sleep ${DELAY_TIME}

chmod 664 *.FTS
chmod 664 *.log

L0_TARNAME=${DATE}.comp.l0.tgz
tar czf ${L0_TARNAME} *.FTS *.log
tar tvfz ${L0_TARNAME} > ${DATE}.comp.l0.tarlist

if [ -h ${HPSS_GATEWAY}/${L0_TARNAME} ]; then
  rm ${HPSS_GATEWAY}/${L0_TARNAME}
fi

ln -s ${DIR}/${L0_TARNAME} ${HPSS_GATEWAY}
