#!/usr/bin/env python3

import functools

if __name__ == "__main__":
    input = [line.strip() for line in open("input.txt", 'r')]
    foldr = lambda f, acc, xs: functools.reduce(lambda x, y: f(y, x), reversed(xs), acc)
    reduce = lambda poly: foldr(lambda x, ys: ys[1:] if len(ys) > 0 and (x != ys[0] and x.swapcase() == ys[0]) else x + ys, '', poly).strip()
    res = reduce(input[0])
    print('Part1:', len(res))
    
    scores = []
    alphabet = map(chr, range(ord('a'), ord('z')+1))
    for letter in alphabet:
      scores.append((letter, len(reduce(list(filter(lambda l: letter != l.lower(), input[0]))))))

    print('Part2:', (min(scores, key=lambda t: t[1])[1]))
