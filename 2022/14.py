#!/usr/bin/python3
from collections import defaultdict
from copy import deepcopy
dat = [
	[(490,23),(490,17),(490,23),(492,23),(492,14),(492,23),(494,23),(494,20),(494,23),(496,23),(496,18),(496,23),(498,23),(498,19),(498,23),(500,23),(500,20),(500,23),(502,23),(502,17),(502,23),(504,23),(504,14),(504,23),(506,23),(506,18),(506,23),(508,23),(508,15),(508,23)],[...snip...]
]
grid = defaultdict(int)
points = [p for path in dat for p in path]
minx = min(x for x,y in points)
maxx = max(x for x,y in points)
miny = min(y for x,y in points)
maxy = max(y for x,y in points)
grid = defaultdict(int)
for path in dat:
	x1, y1 = path[0]
	for x2, y2 in path[1:]:
		if x1 == x2:
			for y in range(min(y1,y2), max(y1,y2)+1):
				grid[x1, y] = 1
		elif y1 == y2:
			for x in range(min(x1,x2), max(x1,x2)+1):
				grid[x, y1] = 1
		else:
			raise ValueError("Non-rect path")
		x1, y1 = x2, y2
initgrid = deepcopy(grid)

def dropsand(x, y, partB):
	global miny
	while True:
		if y >= maxy and not partB:
			return False
		elif y >= maxy + 1 and partB:
			grid[x, y] = 2
			return True
		elif grid[x, y+1] == 0:
			y += 1
			continue
		elif grid[x-1, y+1] == 0:
			y += 1
			x -= 1
			continue
		elif grid[x+1, y+1] == 0:
			y += 1
			x += 1
			continue
		else:
			if y < miny:
				miny = y
			grid[x, y] = 2
			return True

i = 0
while dropsand(500, miny - 1, False):
	i += 1
print(i)

grid = initgrid

i = 0
while grid[500, 0] == 0:
	dropsand(500, 0, True)
	i += 1
print(i)
