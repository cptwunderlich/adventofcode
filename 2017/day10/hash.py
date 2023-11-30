#!/usr/bin/env python3

from collections import deque
from itertools import cycle, islice
from functools import reduce
from operator import xor

def hash(src, ns, i, skip):
    for n in ns:
        d = deque(src)
        d.rotate(-(i%len(src)))
        tmp = list(reversed(list(islice(cycle(d), 0, n))))
        for j in range(0, n):
            src[(i+j)%len(src)] = tmp[j]
        i += n + skip
        skip += 1

    return src, i, skip

if __name__ == '__main__':
    with open('input.txt') as f:
        for line in f:
            ns = [int(x) for x in line.split(',')]
            ns2 = [ord(x) for x in line.strip()]

    ns2 += [17, 31, 73, 47, 23]

    length = 256
    src = list(range(0, length))
    i = 0
    skip = 0

    src, i, skip = hash(src, ns, i, skip)

    print("Part 1:", src[0] * src[1])

    src = list(range(0, length))
    i = 0
    skip = 0

    for _ in range(0, 64):
        src, i, skip = hash(src, ns2, i, skip)

    knothash = ''
    for i in list(range(0, length, 16)):
        knothash += format(reduce(xor, src[i:(i+16)]), '02x')

    print("Part 2:", knothash)
