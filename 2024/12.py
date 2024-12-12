#!/usr/bin/python
from collections import defaultdict, Counter
from fractions import Fraction
from copy import deepcopy
from pprint import pprint
import re, math
from itertools import count, cycle, repeat, chain, groupby, product, permutations, combinations
from functools import cache, cmp_to_key, partial, reduce

dat = """KKKKKKKKKKKEEEEEEEEEEWWWWWWWWWWWWUWWUUHSSSHNNNNP[...]""".split("\n")

#dat = "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE".split("\n")

CY = len(dat)
CX = len(dat[0])

regions = []
seen = set()
def doregion(x, y):
	c = dat[y][x]
	todo = [(x,y)]
	region = set()
	while todo:
		x,y = todo.pop(0)
		if (x,y) in seen:
			continue
		seen.add((x,y))
		region.add((x,y))
		for nx,ny in [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]:
			if 0 <= nx < CX and 0 <= ny < CY and dat[ny][nx] == c:
				todo.append((nx, ny))
	regions.append((c, region))
for y in range(CY):
	for x in range(CX):
		if (x,y) not in seen:
			doregion(x, y)

def perimeter(region):
	n = 0
	for x,y in region:
		for nx,ny in [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]:
			if (nx,ny) not in region:
				n += 1
	return n
def price(region):
	return perimeter(region) * len(region)
print(sum(price(r) for c,r in regions))

def perimeter2(region):
	n = 0
	for x,y in region:
		for nx,ny,x1,y1,x2,y2 in [(x+1,y,x,y-1,x+1,y-1),(x-1,y,x,y-1,x-1,y-1),(x,y+1,x-1,y,x-1,y+1),(x,y-1,x-1,y,x-1,y-1)]:
			if (nx,ny) not in region and not ((x1,y1) in region and (x2,y2) not in region):
				n += 1
	return n
def price2(region):
	return perimeter2(region) * len(region)
print(sum(price2(r) for c,r in regions))
