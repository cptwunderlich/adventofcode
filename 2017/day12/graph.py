#!/usr/bin/env python3

# Advent of Code 2017, Day 12
# Benjamin Maurer (github.com/cptwunderlich)

if __name__ == "__main__":
    with open('input.txt') as f:
        g = ((cons[0], cons[1].split(', ')) for cons in (line.strip().split(' <-> ') for line in f))

        cons = {}
        for con in g:
          cons[con[0]] = con[1]

    visited = set()

    def dfs(nodes):
        for node in nodes:
            if node not in visited:
                visited.add(node)
                dfs(cons[node])

    visited.add('0')
    dfs(cons['0'])

    print("Part 1:", len(visited))

    groups = 1
    for key in cons.keys():
        if key not in visited:
          groups += 1
          dfs(cons[key])

    print("Part 2:", groups)
