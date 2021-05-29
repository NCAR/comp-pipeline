#!/bin/sh

DATA_PATH=$1

FILES=$(find ${DATA_PATH} -name '*.FTS')
for f in ${FILES}; do
  s=$(stat -c %s $f)
  echo "$(basename $f) $s"
done
