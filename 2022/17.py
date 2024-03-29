#!/usr/bin/python3
from io import StringIO
dat = ">>>><>>>><>>>[...snip...]"
#dat = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
datpos = 0

shapes = [
	[[1,1,1,1]],
	[[0,1,0],[1,1,1],[0,1,0]],
	[[1,1,1],[0,0,1],[0,0,1]],
	[[1],[1],[1],[1]],
	[[1,1],[1,1]],
]

well = set()
wellheight = 0
wellwidth = 7
def doesoverlap(i, x, y):
	for dy in range(len(shapes[i])):
		for dx in range(len(shapes[i][dy])):
			if shapes[i][dy][dx] and (x+dx,y+dy) in well:
				return True
	return False
def drop(i):
	global wellheight, datpos
	x = 2
	y = wellheight + 3
	while True:
		d = dat[datpos]
		datpos = (datpos + 1) % len(dat)

		oldx = x
		if d == '<':
			x -= 1
		else:
			x += 1
		if x < 0 or x + len(shapes[i][0]) > wellwidth or doesoverlap(i, x, y):
			x = oldx

		y -= 1
		if y < 0 or doesoverlap(i, x, y):
			y += 1
			break

	for dy in range(len(shapes[i])):
		for dx in range(len(shapes[i][dy])):
			if shapes[i][dy][dx]:
				well.add((x + dx, y + dy))
				if y + dy + 1 > wellheight:
					wellheight = y + dy + 1

for i in range(2022):
	drop(i % len(shapes))
print(wellheight)

well = set()
wellheight = 0
datpos = 0

def getwellstate(i):
	a = []
	for x in range(wellwidth):
		y = wellheight
		while (x,y) not in well and y >= 0:
			y -= 1
		a.append(wellheight - y)
	a.append(datpos)
	a.append(i)
	return tuple(a)

states = {}
wellheights = [0]
i = 0
while True:
	drop(i % len(shapes))
	state = getwellstate(i % len(shapes))
	i += 1
	wellheights.append(wellheight)
	if state in states:
		oldi, oldwellheight = states[state]
		n = 1000000000000 - oldi
		loops, rest = divmod(n, i - oldi)
		res = wellheights[oldi + rest] + loops * (wellheight - oldwellheight)
		print(res)
		break
	else:
		states[state] = i, wellheight
