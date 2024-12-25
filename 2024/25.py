#!/usr/bin/python
from aocimports import *

dat = """.....
.....
[...]"""

#dat = "#####\n.####\n.####\n.####\n.#.#.\n.#...\n.....\n\n#####\n##.##\n.#.##\n...##\n...#.\n...#.\n.....\n\n.....\n#....\n#....\n#...#\n#.#.#\n#.###\n#####\n\n.....\n.....\n#.#..\n###..\n###.#\n###.#\n#####\n\n.....\n.....\n.....\n#....\n#.#..\n#.#.#\n#####"

dat = [i.split("\n") for i in dat.split("\n\n")]

N = len(dat)
CX = 5
CY = 7

locks = []
keys = []
for i in dat:
	if all(c == "#" for c in i[0]):
		target = locks
	elif all(c == "#" for c in i[-1]):
		target = keys
	else:
		1/0
	newentry = [0] * CX
	for y, row in enumerate(i):
		for x, c in enumerate(row):
			if c == "#":
				newentry[x] += 1
	target.append(newentry)

n = 0
for l in locks:
	for k in keys:
		if all(i+j <= 7 for i,j in zip(l, k)):
			n += 1
print(n)
