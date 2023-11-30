#!/usr/bin/env python3

from collections import defaultdict

if __name__ == "__main__":
    d = defaultdict(int)
    
    decode = lambda opc: '+' if opc == 'inc' else '-'    
    highest = 0    

    with open("input.txt") as f:
        g = (line.split() for line in f)
        for instr in g:
            r1, opc, op, ifkw, r2, opc2, op2 = instr
            exec('if d["{}"] {} {}: d["{}"] {}= {}'
                .format(r2, opc2, op2, r1, decode(opc), op))
            highest = max(max(d.values()), highest)

    print("Max value: ", max(d.values()), "\nHighest ever: ", highest)
