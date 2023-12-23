#!/usr/bin/python3
dat = [
	('U', 2),('D', 2),('L', 2),('R', 2),[...snip...]
]
hx = hy = tx = ty = 0
visited = {(0,0)}
def updtail():
	global tx, ty
	if tx == hx and ty < hy - 1:
		ty = hy - 1
	elif tx == hx and ty > hy + 1:
		ty = hy + 1
	elif ty == hy and tx < hx - 1:
		tx = hx - 1
	elif ty == hy and tx > hx + 1:
		tx = hx + 1
	elif tx != hx and ty != hy:
		dx = hx - tx
		dy = hy - ty
		adx, ady = abs(dx), abs(dy)
		sdx, sdy = dx // abs(dx), dy // abs(dy)
		if adx >= 2 or ady >= 2:
			tx += sdx
			ty += sdy
	visited.add((tx, ty))
for d, n in dat:
	if d == 'U':
		for i in range(n):
			hy -= 1
			updtail()
	elif d == 'D':
		for i in range(n):
			hy += 1
			updtail()
	elif d == 'L':
		for i in range(n):
			hx -= 1
			updtail()
	elif d == 'R':
		for i in range(n):
			hx += 1
			updtail()
	else:
		1/0
print(len(visited))
