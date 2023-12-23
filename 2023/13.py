#!/usr/bin/python
dat = """\
.####.#...##.#.
...###.##.#.#..
[...snip...]"""

#dat = "#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.\n\n#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#"

dat = [i.split("\n") for i in dat.split("\n\n")]

def analyse(grid):
	cy = len(grid)
	cx = len(grid[0])

	for y in range(1,cy):
		match = True
		refllen = min(y, cy - y)
		for y2 in range(y-refllen, y+refllen):
			y3 = y + (y - y2 - 1)
			if grid[y2] != grid[y3]:
				match = False
				break
		if match:
			return ('y', y)

	for x in range(1, cx):
		match = True
		refllen = min(x, cx - x)
		for y in range(0, cy):
			if grid[y][x-refllen:x] != grid[y][x:x+refllen][::-1]:
				match = False
				break
		if match:
			return ('x', x)

res = 0
for i in dat:
	axis, n = analyse(i)
	if axis == 'x':
		res += n
	else:
		res += 100 * n
print(res)

def errs(a, b):
	return sum(1 for i,j in zip(a,b) if i != j)

def analyse2(grid):
	cy = len(grid)
	cx = len(grid[0])

	for y in range(1,cy):
		errcount = 0
		refllen = min(y, cy - y)
		for y2 in range(y-refllen, y):
			y3 = y + (y - y2 - 1)
			errcount += errs(grid[y2], grid[y3])
			if errcount > 1:
				break
		if errcount == 1:
			return ('y', y)

	for x in range(1, cx):
		errcount = 0
		refllen = min(x, cx - x)
		for y in range(0, cy):
			errcount += errs(grid[y][x-refllen:x], grid[y][x:x+refllen][::-1])
			if errcount > 1:
				break
		if errcount == 1:
			return ('x', x)

res = 0
for i in dat:
	axis, n = analyse2(i)
	if axis == 'x':
		res += n
	else:
		res += 100 * n
print(res)
