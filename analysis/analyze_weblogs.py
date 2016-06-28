#!/usr/bin/env python

from collections import defaultdict
import csv
import os

LOG_DIR = '/hao/web/logs'
LOG_NAME = os.path.join(LOG_DIR, 'mlso.hao.ucar.edu_access_log')


def main():
    with open(LOG_NAME, 'r') as file:
        reader = csv.reader(file, delimiter=' ')
        clients = defaultdict(int)

        for row in reader:
            c = row[-1]
            if c.startswith('IDL'):
                print row[5]
                clients[c] += 1

    for c, count in sorted(clients.iteritems(), key=lambda (k, v): (v, k), reverse=True):
        print '%8d: %s' % (count, c)


if __name__ == '__main__':
    main()
