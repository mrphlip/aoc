#!/usr/bin/python
from aocimports import *

dat = """#############################################################################################################################################
#...#...#.........#...###...#...###..[...]"""

#dat = "###############\n#...#...#.....#\n#.#.#.#.#.###.#\n#S#...#.#.#...#\n#######.#.#.###\n#######.#.#...#\n#######.#.###.#\n###..E#...#...#\n###.#######.###\n#...###...#...#\n#.#####.#.###.#\n#.#...#.#.#...#\n#.#.#.#.#.#.###\n#...#...#...###\n###############"

dat = dat.split("\n")
CY = len(dat)
CX = len(dat[0])

sx = sy = ex = ey = None
for y, row in enumerate(dat):
	for x, c in enumerate(row):
		if c == "S":
			sx, sy = x, y
		elif c == "E":
			ex, ey = x, y
dat = [[c == "#" for c in row] for row in dat]

#def solve(cheatfrom, cheatto):
#	def neighbours(pos):
#		x, y = pos
#		for dx, dy in [(-1, 0), (0, -1), (0, 1), (1, 0)]:
#			if 0 <= x + dx < CX and 0 <= y + dy < CY and not dat[y+dy][x+dx]:
#				yield (x + dx, y + dy), 1
#		if pos == cheatfrom:
#			yield cheatto, 2
#	dist_map = dijkstra((sx, sy), neighbours, (ex, ey))
#	return dist_map[ex, ey][0]

def cheatopts():
	for y in range(CY):
		for x in range(CX):
			if not dat[y][x]:
				for dx, dy in [(-1, 0), (0, -1), (0, 1), (1, 0)]:
					if 0 <= x + dx < CX and 0 <= y + dy < CY and dat[y+dy][x+dx]:
						for dx2, dy2 in [(-1, 0), (0, -1), (0, 1), (1, 0)]:
							if 0 <= x + dx + dx2 < CX and 0 <= y + dy + dy2 < CY and not dat[y+dy+dy2][x+dx+dx2] and not (dx2 == -dx and dy2 == -dy):
								yield x, y, x+dx+dx2, y+dy+dy2

dist_map_start = dijkstra_grid((sx, sy), CX, CY, lambda x,y: not dat[y][x])
dist_map_end = dijkstra_grid((ex, ey), CX, CY, lambda x,y: not dat[y][x])
target = dist_map_start[ex, ey][0] - 100
n = 0
for x1, y1, x2, y2 in cheatopts():
	if dist_map_start[x1, y1][0] + dist_map_end[x2, y2][0] + 2 <= target:
		n += 1
print(n)

def cheatopts2(maxdist):
	for y in range(CY):
		for x in range(CX):
			if not dat[y][x]:
				for dy in range(-maxdist, maxdist+1):
					if 0 <= y+dy < CY:
						for dx in range(-maxdist + abs(dy), maxdist+1 - abs(dy)):
							if 0 <= x+dx < CX:
								if not dat[y+dy][x+dx] and not (dx == 0 and dy == 0):
									yield x, y, x+dx, y+dy
n = 0
for x1, y1, x2, y2 in cheatopts2(20):
	if dist_map_start[x1, y1][0] + dist_map_end[x2, y2][0] + abs(x1-x2) + abs(y1-y2) <= target:
		n += 1
print(n)
