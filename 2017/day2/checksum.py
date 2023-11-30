#!/usr/bin/env python3

import sys

def min_max_diff(iterable):
    l = list(iterable)
    return max(l) - min(l)

def int_rows(f):
    return (map(int, l) for l in (row.split() for row in f))

def calc_checksum(filename):
    with open(filename) as f:
        return sum(map(min_max_diff, int_rows(f)))

def f(x, xs):
    l = list(filter(lambda b: x % b == 0, xs))
    return (l and [x]+l or [])

def ediv_row(iterable):
    ls = sorted(list(iterable), reverse=True)
    g = lambda xs, ys: xs and xs or f(ys[0], ys[1:])
    xs = []
    for i in range(0, len(ls)-1):
        xs = g(xs, ls[i:])
    return xs

def evenly_divide(filename):
    with open(filename) as f:
        return sum(map(lambda x: x and x[0] / x[1] or 0, (map(ediv_row, int_rows(f)))))

if __name__ == "__main__":
    if len(sys.argv) > 1:
        filename = sys.argv[1]
    else:
        filename = "day2spreadsheet"

    print("Checksum", calc_checksum(filename))
    print("Checksum2", evenly_divide(filename))
