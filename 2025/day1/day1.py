#!/bin/env python3

from functools import reduce
import sys

def parse(line):
    r = int(line[1:])
    return r if line[0] == 'R' else -r

def part1(state, rot):
    acc, cnt = state
    acc = (acc + rot) % 100
    if acc == 0:
        cnt += 1
    return (acc, cnt)

def part2(state, rot):
    acc, cnt = state
    quot, acc2 = divmod(acc + rot, 100)
    zeroes = abs(quot) - (1 if rot < 0 and acc == 0 else 0) + (1 if rot < 0 and acc2 == 0 else 0)
    return (acc2, cnt + zeroes)

if __name__ == '__main__':
    lines = [parse(line.strip()) for line in sys.stdin]
    solution1 = reduce(part1, lines, (50, 0))[1]
    print('Part1:', solution1)
    solution2 = reduce(part2, lines, (50, 0))[1]
    print('Part2:', solution2)

