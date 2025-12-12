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

dat = "###\n##.\n##.\n\n###\n##.\n.##\n\n.##\n###\n##.\n\n##.\n###\n##.\n\n###\n#..\n###\n\n###\n.#.\n###"; dat2 = [((4,4),(0,0,0,0,2,0)),((12,5),(1,0,1,0,2,2)),((12,5),(1,0,1,0,3,2)),]

dat = [[[c == "#" for c in row] for row in i.split("\n")] for i in dat.split("\n\n")]

from pysat.formula import Atom, And, Or, Neg
from pysat.solvers import Solver

def cell(piece, x, y, orient):
	if orient & 1:
		x = 2 - x
	if orient & 2:
		y = 2 - y
	if orient & 4:
		x, y = y, x
	return dat[piece][y][x]

def solve(row):
	(w, h), counts = row
	totcount = sum(counts)

	def gen():
		for i, n in enumerate(counts):
			for x in range(n):
				yield i
	piecelist = list(gen())

	atoms = {
		(ix, x, y, orient): Atom(f"{ix}-{x}-{y}-{orient}")
		for ix in range(totcount) for x in range(w-2) for y in range(h-2) for orient in range(8)
	}

	conds = []

	# each piece is in a place
	for ix in range(totcount):
		conds.append(Or(*[atoms[ix, x, y, orient] for x in range(w-2) for y in range(h-2) for orient in range(8)]))

	# each cell contains at most one piece
	for y in range(h):
		for x in range(w):
			pieces = []
			for ix, piece in enumerate(piecelist):
				for y2 in range(3):
					if not (0 <= y-y2 < h-2):
						continue
					for x2 in range(3):
						if not (0 <= x-x2 < w-2):
							continue
						for orient in range(8):
							if cell(piece, x2, y2, orient):
								pieces.append(atoms[ix, x-x2, y-y2, orient])
			#print(x, y, pieces)
			for i, j in combinations(pieces, 2):
				conds.append(Neg(And(i, j)))

	#pprint(conds)
	with Solver(bootstrap_with=And(*conds)) as solver:
		return solver.solve()

for row in dat2:
	print(solve(row))
