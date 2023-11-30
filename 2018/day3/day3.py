#!/usr/bin/env python3

from collections import namedtuple
import re

def overlap(min1, max1, min2, max2):
    return max(0, min(max1, max2) - max(min1, min2))

def day1n2(lines):
    Claim = namedtuple('Claim', ['id', 'x', 'y', 'w', 'h'])
    claims = []

    for line in lines:
        claims.append(Claim(*map(int, re.findall(r'\d+', line))))             
        
    area = [[0 for x in range(0,1000)] for y in range(0,1000)]
        
    for claim in claims:
        for y in range(claim.y, (claim.y + claim.h)):
            for x in range(claim.x, (claim.x + claim.w)):                
                area[y][x] += 1
    
    overlaps = 0
    unique_claim = 0
    for row in area:
        overlaps += sum(1 for a in row if a > 1)

    for claim in claims:
        unique = True
        for y in range(claim.y, (claim.y + claim.h)):
            for x in range(claim.x, (claim.x + claim.w)):
                if area[y][x] > 1:
                    unique = False
                    break
                
            if not unique: break
            
        if unique:
            unique_claim = claim.id
            break
    
    print("Part1: Overlaps: ", overlaps)
    print("Part2: Unique: ", unique_claim)

if __name__ == "__main__":

    input = [line.strip() for line in open("input.txt", 'r')]
    day1n2(input)
