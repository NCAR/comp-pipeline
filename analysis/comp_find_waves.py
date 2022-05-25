#!/usr/bin/env python

import datetime
import glob
import os

MAX_INTERVAL = datetime.timedelta(seconds=90)


def get_time(line):
    tokens = line.split()
    raw_filename = tokens[0]
    return(datetime.datetime.strptime(raw_filename[:15], "%Y%m%d.%H%M%S"))


def find_max_continuous_files(catalog):
    times = [get_time(line) for line in catalog]
    t0 = times[0]
    short_intervals = []
    for t in times[1:]:
        short_intervals.append(t - t0 < MAX_INTERVAL)
        t0 = t
    max_continuous_files = 0
    current_continuous_files = 0
    for s in short_intervals:
        if s:
            current_continuous_files += 1
            max_continuous_files = max(max_continuous_files, current_continuous_files)
        else:
            current_continuous_files = 0
    if max_continuous_files > 0:
        max_continuous_files += 1
    return(max_continuous_files)


def main():
    process_root = "/hao/dawn/Data/CoMP/process"
    wave_type = "1074"

    dates = glob.glob(os.path.join(process_root, "????????"))
    dates = [os.path.basename(d) for d in dates]
    dates = sorted(dates)
    for d in dates:
        basename = f"{d}.comp.{wave_type}.good.waves.files.txt"
        filename = os.path.join(process_root, d, "level1", basename)
        try:
            with open(filename, "r") as f:
                catalog = f.readlines()
            if len(catalog) > 0:
                max_continuous_files = find_max_continuous_files(catalog)
                print(f"{d}: {len(catalog)} good wave files, {max_continuous_files} continuous files")
        except FileNotFoundError:
            pass


if __name__ == "__main__":
    main()
