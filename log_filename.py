#!/usr/bin/env python

import argparse
import os
import sys

PY3 = sys.version_info[0] == 3

if PY3:
    import configparser
else:
    import ConfigParser as configparser


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Log filename retriever')
    parser.add_argument('date', type=str, help='date')
    parser.add_argument('config_filename', type=str, help='config filename')
    args = parser.parse_args()

    config = configparser.ConfigParser()
    config.read(args.config_filename)
    log_dir = config.get('log', 'log_dir')
    log_filename = os.path.join(log_dir, '%s.log' % args.date)
    print(log_filename)