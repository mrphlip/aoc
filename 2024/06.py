#!/usr/bin/python
from collections import defaultdict, Counter
from fractions import Fraction
from copy import deepcopy
from pprint import pprint
import re, math

dat = """..................#.....[...]""".split("\n")

CX = len(dat)
CY = len(dat[0])

sx = sy = None
for y, row in enumerate(dat):
	for x, c in enumerate(row):
		if c == "^":
			sx, sy = x, y
			break
dat = [[c == "#" for c in row] for row in dat]

def go():
	dx, dy = 0, -1
	px, py = sx, sy
	seen = {(px, py)}
	seen_dir = set()
	while True:
		if not (0 <= px+dx < CX and 0 <= py+dy < CY):
			return len(seen)
		if (px, py, dx, dy) in seen_dir:
			return None
		seen_dir.add((px, py, dx, dy))
		if dat[py+dy][px+dx]:
			dx, dy = -dy, dx
		else:
			px += dx
			py += dy
			seen.add((px, py))

print(go())

n = 0
for y in range(CY):
	#print(y)
	for x in range(CX):
		if dat[y][x] or (x == sx and y == sy):
			continue
		dat[y][x] = True
		if go() is None:
			n += 1
		dat[y][x] = False
print(n)
