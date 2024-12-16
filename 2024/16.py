#!/usr/bin/python
import sys, os
sys.path.insert(1, os.path.join(os.path.dirname(__file__), "..", "utils"))
from collections import defaultdict, Counter
from fractions import Fraction
from copy import deepcopy
from pprint import pprint
import re
from math import factorial as fact, gcd, lcm, isqrt, perm as npr, comb as ncr, prod
from math import sqrt, sin, cos, tan, atan, pi, ceil, floor
from itertools import count, cycle, repeat, chain, groupby, product, permutations, combinations
from functools import cache, cmp_to_key, partial, reduce

from myutils import crt, gcdext, primes, factor
from matrix import Matrix
from vector import Vector
from modular import Modular

dat = """#################[...]"""

#dat = "###############\n#.......#....E#\n#.#.###.#.###.#\n#.....#.#...#.#\n#.###.#####.#.#\n#.#.#.......#.#\n#.#.#####.###.#\n#...........#.#\n###.#.#####.#.#\n#...#.....#.#.#\n#.#.#.###.#.#.#\n#.....#...#.#.#\n#.###.#.#.#.#.#\n#S..#.....#...#\n###############"

dat = dat.split("\n")
CY = len(dat)
CX = len(dat[0])

dat = [list(row) for row in dat]
sx = sy = ex = ey = None
for y, row in enumerate(dat):
	for x, c in enumerate(row):
		if c == "S":
			sx, sy = x, y
		elif c == "E":
			ex, ey = x, y
dat = [[c == "#" for c in row] for row in dat]

def solve():
	bests = {(sx, sy, 1, 0): 0}
	bestpaths = defaultdict(set)
	bestpaths[sx, sy, 1, 0] = {(sx, sy)}
	solved = set()
	todo = {(sx, sy, 1, 0)}
	while todo:
		x, y, dx, dy = min(todo, key=bests.__getitem__)
		todo.remove((x, y, dx, dy))
		solved.add((x, y, dx, dy))
		cost = bests[x, y, dx, dy]
		nx, ny = x + dx, y + dy
		i = 1
		while 0 <= nx < CX and 0 <= ny < CY and not dat[ny][nx]:
			if (nx, ny, dx, dy) not in solved and ((nx, ny, dx, dy) not in bests or bests[nx,ny,dx,dy] > cost + i):
				bests[nx, ny, dx, dy] = cost + i
				bestpaths[nx, ny, dx, dy] = set(bestpaths[x, y, dx, dy])
				bestpaths[nx, ny, dx, dy].add((nx, ny))
				todo.add((nx, ny, dx, dy))
			elif (nx, ny, dx, dy) in bests and bests[nx,ny,dx,dy] == cost + i:
				bestpaths[nx, ny, dx, dy].update(bestpaths[x, y, dx, dy])
			nx += dx
			ny += dy
			i += 1
		rx, ry = dy, -dx
		if (x, y, rx, ry) not in solved and ((x, y, rx, ry) not in bests or bests[x,y,rx,ry] >= cost + 1000):
			bests[x, y, rx, ry] = cost + 1000
			bestpaths[x, y, rx, ry] = set(bestpaths[x, y, dx, dy])
			bestpaths[x, y, rx, ry].add((x, y))
			todo.add((x, y, rx, ry))
		elif (x, y, rx, ry) in bests and bests[x, y, rx, ry] == cost + 1000:
			bestpaths[x, y, rx, ry].update(bestpaths[x, y, dx, dy])
		rx, ry = -dy, dx
		if (x, y, rx, ry) not in solved and ((x, y, rx, ry) not in bests or bests[x,y,rx,ry] >= cost + 1000):
			bests[x, y, rx, ry] = cost + 1000
			bestpaths[x, y, rx, ry] = set(bestpaths[x, y, dx, dy])
			bestpaths[x, y, rx, ry].add((x, y))
			todo.add((x, y, rx, ry))
		elif (x, y, rx, ry) in bests and bests[x, y, rx, ry] == cost + 1000:
			bestpaths[x, y, rx, ry].update(bestpaths[x, y, dx, dy])
		if (ex, ey, -1, 0) in solved and (ex, ey, 1, 0) in solved and (ex, ey, 0, -1) in solved and (ex, ey, 0, 1) in solved:
			break
	#return min(bests[ex, ey, i, j] for i, j in [(-1, 0), (1, 0), (0, -1), (0, 1)])
	m = min(bests[ex, ey, i, j] for i, j in [(-1, 0), (1, 0), (0, -1), (0, 1)])
	n = set.union(*[bestpaths[ex, ey, i, j] for i, j in [(-1, 0), (1, 0), (0, -1), (0, 1)] if bests[ex, ey, i, j] == m])
	return m, len(n)

print(solve())
