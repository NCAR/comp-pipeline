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

tar czf ${DATE}.comp.l0.tgz *.FTS *.log
tar tvfz ${DATE}.comp.l0.tgz > ${DATE}.comp.l0.tarlist

ln -s ${DIR}/${DATE}.comp.l0.tgz ${HPSS_GATEWAY}
