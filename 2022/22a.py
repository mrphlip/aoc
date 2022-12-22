#!/usr/bin/python3
dat = """\
                                                  .#..#.....#.....................................................................#...........#.......
                                                  ................#.....#......#...#................#..........#.#...........#........................
                                                  etc""".split("\n")
dat2 = [
	7, 'R', 13, 'R', etc
]
#dat = """        ...#\n        .#..\n        #...\n        ....\n...#.......#\n........#...\n..#....#....\n..........#.\n        ...#....\n        .....#..\n        .#......\n        ......#.""".split("\n")
#dat2 = [10,'R',5,'L',5,'R',10,'L',4,'R',5,'L',5]

x = len([i for i in dat[0] if i == ' '])
y = 0
d = 0
def ob():
	return y < 0 or y >= len(dat) or x < 0 or x >= len(dat[y]) or dat[y][x] == ' '
def step():
	global x, y
	oldx, oldy = x, y
	if d == 0: # right
		x += 1
		if ob():
			x = len([i for i in dat[y] if i == ' '])
	elif d == 2: # left
		x -= 1
		if ob():
			x = len(dat[y]) - 1
	elif d == 1: # down
		y += 1
		if ob():
			y = 0
			while ob():
				y += 1
	elif d == 3: # up
		y -= 1
		if ob():
			y = len(dat) - 1
			while ob():
				y -= 1
	if dat[y][x] == '#':
		x, y = oldx, oldy
print(x,y,d)
for i in range(0, len(dat2), 2):
	for j in range(dat2[i]):
		step()
	if i+1 < len(dat2):
		if dat2[i+1] == 'L':
			d = (d - 1) % 4
		else:
			d = (d + 1) % 4
	print(x,y,d)

print(1000 * (y+1) + 4 * (x+1) + d)
