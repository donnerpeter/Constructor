#!/usr/bin/python3
# from http://stackoverflow.com/questions/5535512/how-to-hack-ghci-or-hugs-so-that-it-prints-unicode-chars-unescaped/5615374#5615374

import sys
import re

def tr(match):
    s = match.group(1)
    try:
        return chr(int(s))
    except ValueError:
        return s

for line in sys.stdin:
    sys.stdout.write(re.sub(r"\\([0-9]{4})", tr, line))
