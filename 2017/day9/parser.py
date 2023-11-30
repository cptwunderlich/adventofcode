#!/usr/bin/env python3

def curly_open():
    m['lvl'] += 1

def curly_close():
    m['score'] += m['lvl']
    m['lvl'] -= 1

def angle_open():
    m['ignore'] = True

def angle_close():
    m['ignore'] = False

if __name__ == "__main__":
    igncnt = 0
    m = {'ignore': False, 'neg': False, 'lvl': 0, 'score': 0}
    with open("./input.txt") as f:
        g = (line for line in f)
        handlers = {'{': curly_open,
                    '}': curly_close,
                    '<': angle_open,
                    '>': angle_close}
        for line in g:
            for c in line:
                if m['neg'] or c == '!':
                    m['neg'] = not m['neg']
                elif m['ignore'] and c != '>':
                    igncnt += 1
                else:
                    handlers.get(c, lambda: 0)()

    print("Score:", m['score'], "ignored:", igncnt)
                
