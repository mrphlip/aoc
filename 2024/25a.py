#!/usr/bin/python
from aocimports import *

dat = """.....
.....
[...]"""

#dat = "#####\n.####\n.####\n.####\n.#.#.\n.#...\n.....\n\n#####\n##.##\n.#.##\n...##\n...#.\n...#.\n.....\n\n.....\n#....\n#....\n#...#\n#.#.#\n#.###\n#####\n\n.....\n.....\n#.#..\n###..\n###.#\n###.#\n#####\n\n.....\n.....\n.....\n#....\n#.#..\n#.#.#\n#####"

dat = [i.split("\n") for i in dat.split("\n\n")]

dat = [{(x,y) for y, row in enumerate(i) for x, c in enumerate(row) if c == "#"} for i in dat]
n = 0
for a, b in combinations(dat, 2):
	if not a&b:
		n += 1
print(n)
