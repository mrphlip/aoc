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

dat = """##################################################
#...O.#O#O[...]"""

dat2 = """^<<<^^^vv>^^<^^^v<[...]"""

#dat = "########\n#..O.O.#\n##@.O..#\n#...O..#\n#.#.O..#\n#...O..#\n#......#\n########"
#dat2 = "<^^>>>vv<v>>v<<"
#dat = "##########\n#..O..O.O#\n#......O.#\n#.OO..O.O#\n#..O@..O.#\n#O#..O...#\n#O..O..O.#\n#.OO.O.OO#\n#....O...#\n##########"
#dat2 = "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\nvvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\nv^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

dirs = {"<": (-1,0), ">": (1, 0), "^": (0, -1), "v": (0, 1)}

dat = dat.split("\n")
#dat2 = dat2.split("\n")
dat2 = dat2.replace("\n", "")

CX = len(dat[0])
CY = len(dat)

dat = [list(row) for row in dat]

sx, sy = None, None
for y, row in enumerate(dat):
	for x, c in enumerate(row):
		if c == "@":
			dat[y][x] = "."
			sx, sy = x, y
			break

def dostep(grid, x, y, step):
	dx, dy = dirs[step]
	i = 1
	while grid[y+i*dy][x+i*dx] == "O":
		i += 1
	if grid[y+i*dy][x+i*dx] == "#":
		return x,y
	if i > 1:
		grid[y+i*dy][x+i*dx] = "O"
		grid[y+dy][x+dx] = "."
	return x+dx, y+dy
def follow(steps):
	grid = deepcopy(dat)
	x, y = sx, sy
	for step in steps:
		x, y = dostep(grid, x, y, step)
	return grid, x, y

def score(grid):
	n = 0
	for y, row in enumerate(grid):
		for x, c in enumerate(row):
			if c == "O":
				n += y*100+x
	return n

n = 0
#for steps in dat2:
#	n += score(follow(steps)[0])
n = score(follow(dat2)[0])
print(n)

def dostep2(grid, x, y, step):
	boxes = []
	dx, dy = dirs[step]
	if grid[y+dy][x+dx] == "#":
		return x, y
	def handlebox(bx, by):
		if grid[by][bx] == '[':
			boxes.append((bx, by))
			handlebox(bx+dx, by+dy)
			if dx != -1:
				handlebox(bx+1+dx, by+dy)
		elif grid[by][bx] == ']':
			boxes.append((bx-1, by))
			if dx != 1:
				handlebox(bx-1+dx, by+dy)
			handlebox(bx+dx, by+dy)
	handlebox(x+dx, y+dy)
	for bx, by in boxes:
		if grid[by+dy][bx+dx] == "#":
			return x, y
		if grid[by+dy][bx+1+dx] == "#":
			return x, y
	for bx, by in boxes:
		grid[by][bx] = "."
		grid[by][bx+1] = "."
	for bx, by in boxes:
		grid[by+dy][bx+dx] = "["
		grid[by+dy][bx+1+dx] = "]"
	return x+dx, y+dy
def follow2(steps):
	grid = []
	for row in dat:
		newrow = []
		for cell in row:
			if cell == 'O':
				newrow.extend("[]")
			else:
				newrow.extend([cell, cell])
		grid.append(newrow)
	x, y = sx*2, sy
	for step in steps:
		x, y = dostep2(grid, x, y, step)
	return grid, x, y

def score2(grid):
	n = 0
	for y, row in enumerate(grid):
		for x, c in enumerate(row):
			if c == "[":
				n += y*100+x
	return n

n = 0
grid = follow2(dat2)[0]
#print("\n".join("".join(row) for row in grid))
n = score2(grid)
print(n)
