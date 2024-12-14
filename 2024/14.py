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

dat = [
	((99,12),(19,18)),
	((90,98),(47,-52)),
	[...]
]

CX=101
CY=103

#dat = [((0,4),(3,-3)),((6,3),(-1,-3)),((10,3),(-1,2)),((2,0),(2,-1)),((0,0),(1,3)),((3,0),(-2,-2)),((7,6),(-1,-3)),((3,0),(-1,-2)),((9,3),(2,3)),((7,3),(-1,2)),((2,4),(2,-3)),((9,5),(-3,-3))]
#CX, CY = 11, 7

areas = [0,0,0,0]
for (x,y),(vx,vy) in dat:
	x = (x + 100*vx) % CX
	y = (y + 100*vy) % CY
	n = 0
	if x == CX//2:
		continue
	elif x > CX//2:
		n += 1
	if y == CY//2:
		continue
	elif y > CY//2:
		n += 2
	areas[n] += 1
print(prod(areas))

n, _ = crt(18,CX,76,CY)
print(n)

for i in [n]: #range(1000):
	img = [[0 for x in range(CX)] for y in range(CY)]
	for (x,y),(vx,vy) in dat:
		x = (x + i*vx) % CX
		y = (y + i*vy) % CY
		img[y][x] = 1
	with open(f"{i:04d}.pbm", "w") as fp:
		fp.write(f"P1\n{CX} {CY}\n")
		for row in img:
			for cell in row:
				fp.write(f"{cell} ")
			fp.write("\n")
