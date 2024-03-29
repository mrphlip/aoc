#!/usr/bin/python3
from collections import Counter
dat = """\
.##.####.#.#..###....##.....#.#.#.###.#####..#..#...#..#.##.##.#..#####.#.
#.#..####..#...#.####..#.#..##.###..#.#...#.#..##..##.#...#..#####.#.####.
[...snip...]"".split("\n")
#dat = ".....\n..##.\n..#..\n.....\n..##.\n.....".split("\n")
#dat = "....#..\n..###.#\n#...#.#\n.#...##\n#.###..\n##.#.##\n.#..#..".split("\n")
elves = {(x, y) for y,row in enumerate(dat) for x,cell in enumerate(row) if cell == '#'}

step = 0

def propmove(x, y):
	if all((x+dx,y+dy) not in elves for dx in [-1,0,1] for dy in [-1,0,1] if dx or dy):
		return (x, y)
	for i in range(step, step+4):
		if i%4 == 0 and all((x+dx,y-1) not in elves for dx in [-1,0,1]):
			return (x,y-1)
		if i%4 == 1 and all((x+dx,y+1) not in elves for dx in [-1,0,1]):
			return (x,y+1)
		if i%4 == 2 and all((x-1,y+dy) not in elves for dy in [-1,0,1]):
			return (x-1,y)
		if i%4 == 3 and all((x+1,y+dy) not in elves for dy in [-1,0,1]):
			return (x+1,y)
	return (x, y)

def move():
	global elves, step
	props = [((x, y), propmove(x,y)) for (x,y) in elves]
	counts = Counter(i[1] for i in props)
	elves = {newp if counts[newp] <= 1 else oldp for oldp, newp in props}
	step += 1

for i in range(10):
	move()
minx = min(x for x,y in elves)
maxx = max(x for x,y in elves)
miny = min(y for x,y in elves)
maxy = max(y for x,y in elves)
print((maxx - minx + 1) * (maxy - miny + 1) - len(elves))

i = 10
oldelves = None
while oldelves != elves:
	oldelves = elves
	move()
	i += 1
print(i)
