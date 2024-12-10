#!/usr/bin/python
from collections import defaultdict, Counter
from fractions import Fraction
from copy import deepcopy
from pprint import pprint
import re, math
import itertools

dat = """123454078104569871014321021[...]""".split("\n")

#dat = "89010123\n78121874\n87430965\n96549874\n45678903\n32019012\n01329801\n10456732".split("\n")

CY = len(dat)
CX = len(dat[0])

dat = [[int(i) for i in row] for row in dat]

def eval(x,y):
	seen = set()
	todo = [(x,y)]
	n = 0
	while todo:
		x,y = todo.pop()
		if (x,y) in seen:
			continue
		seen.add((x,y))
		c = dat[y][x]
		if c == 9:
			n += 1
			continue
		for nx,ny in [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]:
			if 0 <= nx < CX and 0 <= ny < CY and dat[ny][nx] == c + 1:
				todo.append((nx, ny))
	return n

n = 0
for y, row in enumerate(dat):
	for x, c in enumerate(row):
		if c == 0:
			n += eval(x,y)
print(n)

def eval2(x,y):
	seen = {}
	todo = [(x,y,None,None)]
	tot = 0
	while todo:
		x,y,fx,fy = todo.pop(0)
		if fx is None:
			n = 1
		else:
			n = seen[fx,fy]
		c = dat[y][x]
		if c == 9:
			tot += n
			continue
		if (x,y) in seen:
			seen[x,y] += n
			continue
		seen[x,y] = n
		for nx,ny in [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]:
			if 0 <= nx < CX and 0 <= ny < CY and dat[ny][nx] == c + 1:
				todo.append((nx, ny, x, y))
	return tot

n = 0
for y, row in enumerate(dat):
	for x, c in enumerate(row):
		if c == 0:
			n += eval2(x,y)
print(n)
