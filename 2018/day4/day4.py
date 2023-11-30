#!/usr/bin/env python3

from collections import defaultdict
import re

def part1(input):
    guards = defaultdict(lambda: [0 for x in range(0, 60)])

    id = 0
    tstart = 0
    for line in input:
        t = int(line[15] + line[16])
        cmd = line[19]

        if cmd == 'G':
            id = int(re.search('.+#(\d+).*', line).group(1))
        elif cmd == 'f':
            tstart = t
        else:
            for i in range(tstart, t):
                guards[id][i] += 1

    guard_stats = list(map(lambda k: (k, sum(guards[k]), max(guards[k])), guards))
    max_guard = max(guard_stats, key=lambda x: x[1])[0]
    i = guards[max_guard].index(max(guards[max_guard]))

    print('Part1: ', i * max_guard)

    maxminute_guard = max(guard_stats, key=lambda x: x[2])
    maxminute = guards[maxminute_guard[0]].index(maxminute_guard[2])
    print('Part2: ', maxminute_guard[0] * maxminute)

if __name__ == "__main__":
    input = [line.strip() for line in open("input.txt", 'r')]
    input.sort()
    part1(input)
