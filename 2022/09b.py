#!/usr/bin/python3
dat = [
	('U', 2),('D', 2),('L', 2),('R', 2),[...snip...]
]
N = 10
p = [[0,0] for i in range(N)]
visited = {(0,0)}
def updtail():
	for i in range(1, N):
		updstep(i)
	visited.add(tuple(p[-1]))
def updstep(i):
	if p[i][0] == p[i-1][0] and p[i][1] < p[i-1][1] - 1:
		p[i][1] = p[i-1][1] - 1
	elif p[i][0] == p[i-1][0] and p[i][1] > p[i-1][1] + 1:
		p[i][1] = p[i-1][1] + 1
	elif p[i][1] == p[i-1][1] and p[i][0] < p[i-1][0] - 1:
		p[i][0] = p[i-1][0] - 1
	elif p[i][1] == p[i-1][1] and p[i][0] > p[i-1][0] + 1:
		p[i][0] = p[i-1][0] + 1
	elif p[i][0] != p[i-1][0] and p[i][1] != p[i-1][1]:
		dx = p[i-1][0] - p[i][0]
		dy = p[i-1][1] - p[i][1]
		adx, ady = abs(dx), abs(dy)
		sdx, sdy = dx // abs(dx), dy // abs(dy)
		if adx >= 2 or ady >= 2:
			p[i][0] += sdx
			p[i][1] += sdy
for d, n in dat:
	if d == 'U':
		for i in range(n):
			p[0][1] -= 1
			updtail()
	elif d == 'D':
		for i in range(n):
			p[0][1] += 1
			updtail()
	elif d == 'L':
		for i in range(n):
			p[0][0] -= 1
			updtail()
	elif d == 'R':
		for i in range(n):
			p[0][0] += 1
			updtail()
	else:
		1/0
print(len(visited))
