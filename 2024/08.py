#!/usr/bin/python
from collections import defaultdict, Counter
from fractions import Fraction
from copy import deepcopy
from pprint import pprint
import re, math
import itertools

dat = """..........W......8..............................4.
............W...................F............1.L..
[...]""".split("\n")

#dat = "............\n........0...\n.....0......\n.......0....\n....0.......\n......A.....\n............\n............\n........A...\n.........A..\n............\n............".split("\n")

CX = len(dat[0])
CY = len(dat)

ants = [(x,y,c) for y,row in enumerate(dat) for x, c in enumerate(row) if c != "."]
grouped = defaultdict(list)
for x,y,c in ants:
	grouped[c].append((x, y))

antis = set()
for grp in grouped.values():
	for i in range(len(grp)-1):
		for j in range(i+1,len(grp)):
			x1,y1 = grp[i]
			x2,y2 = grp[j]
			xa = x1 - (x2 - x1)
			ya = y1 - (y2 - y1)
			xb = x2 - (x1 - x2)
			yb = y2 - (y1 - y2)
			if 0 <= xa < CX and 0 <= ya < CY:
				antis.add((xa, ya))
			if 0 <= xb < CX and 0 <= yb < CY:
				antis.add((xb, yb))
print(len(antis))

antis = set()
for grp in grouped.values():
	for i in range(len(grp)-1):
		for j in range(i+1,len(grp)):
			x1,y1 = grp[i]
			x2,y2 = grp[j]
			dx = x2 - x1
			dy = y2 - y1
			n = 0
			while 0 <= x1 + n*dx < CX and 0 <= y1 + n*dy < CY:
				antis.add((x1+n*dx,y1+n*dy))
				n += 1
			n = 0
			while 0 <= x1 + n*dx < CX and 0 <= y1 + n*dy < CY:
				antis.add((x1+n*dx,y1+n*dy))
				n -= 1
print(len(antis))
