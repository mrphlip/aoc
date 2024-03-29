#!/usr/bin/python
dat = """\
...................#.............#...........................................#..............................................................
.......................................#.........#..................................................................#.........#........#....
[...snip...]""".split("\n")

CY = len(dat)
CX = len(dat[0])

blankrows = set()
blankcols = set()

#GAPSIZE = 2
GAPSIZE = 1_000_000

for y in range(CY):
	if all(dat[y][x] == '.' for x in range(CX)):
		blankrows.add(y)
for x in range(CX):
	if all(dat[y][x] == '.' for y in range(CY)):
		blankcols.add(x)

galaxies = []
for y in range(CY):
	for x in range(CX):
		if dat[y][x] == '#':
			x2 = x + len([i for i in blankcols if i < x]) * (GAPSIZE-1)
			y2 = y + len([i for i in blankrows if i < y]) * (GAPSIZE-1)
			galaxies.append((x2,y2))

l = 0
for i in range(len(galaxies)):
	for j in range(i+1, len(galaxies)):
		x1, y1 = galaxies[i]
		x2, y2 = galaxies[j]
		l += abs(x1-x2) + abs(y1-y2)
print(l)
