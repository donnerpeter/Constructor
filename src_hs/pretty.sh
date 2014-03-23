#!/usr/bin/python3

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
