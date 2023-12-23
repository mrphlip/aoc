#!/usr/bin/python
from collections import defaultdict

dat = [
	"...........441.................367................296........................................567..47.....45.................947.............",
	"...606..........888.....................508..........*892................+..=138.381..967...............*....%......926...........218.......",
	[...snip...]
]
#dat=["467..114..","...*......","..35..633.","......#...","617*......",".....+.58.","..592.....","......755.","...$.*....",".664.598.."]

CY = len(dat)
CX = len(dat[0])

symbols = {}
numbers = {}
for y in range(CY):
	for x in range(CX):
		if dat[y][x] not in ".0123456789":
			symbols[x,y] = dat[y][x]
		if dat[y][x] in "0123456789":
			if x >= 1 and dat[y][x-1] in "0123456789":
				continue
			for x2 in range(x, CX):
				if dat[y][x2] not in "0123456789":
					x2 -= 1
					break
			numbers[x,y] = (dat[y][x:x2+1], x2+1-x)

def neighbours(x, y, l):
	for y2 in range(y-1,y+2):
		for x2 in range(x-1,x+l+1):
			if not (y2 == y and x <= x2 < x+l):
				if 0 <= x2 < CX and 0 <= y2 < CY:
					yield x2, y2

parts = []
for (x, y), (n, l) in numbers.items():
	if any(p in symbols for p in neighbours(x,y,l)):
		parts.append(int(n))
print(sum(parts))

g = defaultdict(list)
for (x, y), (n, l) in numbers.items():
	for p in neighbours(x, y, l):
		if p in symbols and symbols[p] == '*':
			g[p].append(int(n))
g = [n[0] * n[1] for p,n in g.items() if len(n) == 2]
print(sum(g))
