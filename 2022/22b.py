#!/usr/bin/python3
dat = """\
                                                  .#..#.....#.....................................................................#...........#.......
                                                  ................#.....#......#...#................#..........#.#...........#........................
                                                  etc""".split("\n")
dat2 = [
	7, 'R', 13, 'R', etc
]

#   fe
#  g@@c
#  d@b
# g@@c
# f@a
#  e

x = len([i for i in dat[0] if i == ' '])
y = 0
d = 0
def ob():
	return y < 0 or y >= len(dat) or x < 0 or x >= len(dat[y]) or dat[y][x] == ' '
def step():
	global x, y, d
	oldx, oldy, oldd = x, y, d
	if d == 0: # right
		x += 1
		if ob():
			if 0 <= y < 50:
				x = 99
				y = 149 - y
				d = 2
			elif 50 <= y < 100:
				x = 100 + (y - 50)
				y = 49
				d = 3
			elif 100 <= y < 150:
				x = 149
				y = 149 - y
				d = 2
			elif 150 <= y < 200:
				x = 50 + (y - 150)
				y = 149
				d = 3
	elif d == 2: # left
		x -= 1
		if ob():
			if 0 <= y < 50:
				x = 0
				y = 149 - y
				d = 0
			elif 50 <= y < 100:
				x = y - 50
				y = 100
				d = 1
			elif 100 <= y < 150:
				x = 50
				y = 149 - y
				d = 0
			elif 150 <= y < 200:
				x = 50 + (y - 150)
				y = 0
				d = 1
	elif d == 1: # down
		y += 1
		if ob():
			if 0 <= x < 50:
				x = x + 100
				y = 0
				d = 1
			elif 50 <= x < 100:
				y = 150 + (x - 50)
				x = 49
				d = 2
			elif 100 <= x < 150:
				y = 50 + (x - 100)
				x = 99
				d = 2
	elif d == 3: # up
		y -= 1
		if ob():
			if 0 <= x < 50:
				y = 50 + x
				x = 50
				d = 0
			elif 50 <= x < 100:
				y = 150 + (x - 50)
				x = 0
				d = 0
			elif 100 <= x < 150:
				x = x - 100
				y = 199
				d = 3
	if dat[y][x] == '#':
		x, y, d = oldx, oldy, oldd
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
