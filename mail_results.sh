#!/bin/sh

CONFIG_FILENAME=$1
EMAIL=$2

cat ${CONFIG_FILENAME} | mail -s "Done processing pipeline with $(basename ${CONFIG_FILENAME})" ${EMAIL}