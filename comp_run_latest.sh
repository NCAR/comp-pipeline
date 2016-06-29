#!/bin/sh

DATA_DIR=/export/data1/Data/CoMP

# clean up old latest run
rm -rf $DATA_DIR/logs.latest
rm -rf $DATA_DIR/process.latest

# new latest run
make pipe FLAGS=.latest
