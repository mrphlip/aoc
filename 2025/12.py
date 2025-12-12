#!/usr/bin/python
from aocimports import *

dat = """\
..#
.##
##.

[...]"""

dat2 = [
	((39,43),(31,33,40,22,27,28)),
	((40,37),(39,23,40,36,48,42)),
	[...]
]

#dat = "###\n##.\n##.\n\n###\n##.\n.##\n\n.##\n###\n##.\n\n##.\n###\n##.\n\n###\n#..\n###\n\n###\n.#.\n###"; dat2 = [((4,4),(0,0,0,0,2,0)),((12,5),(1,0,1,0,2,2)),((12,5),(1,0,1,0,3,2)),]

dat = [[[c == "#" for c in row] for row in i.split("\n")] for i in dat.split("\n\n")]
piecesize = [sum(c for row in piece for c in row) for piece in dat]

def solve(row):
	(w, h), counts = row

	totsize = sum(piecesize[ix] * count for ix, count in enumerate(counts))

	return totsize <= w*h

print(sum(solve(row) for row in dat2))
