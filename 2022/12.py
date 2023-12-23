#!/usr/bin/python3
dat = [
	"abaacccccccccccccaaaaaaaccccccccccccccccccccccccccccccccccaaaaaa",
	"abaaccccccccccccccaaaaaaaaaaccccccccccccccccccccccccccccccccaaaa",
	[...snip...]
]
dat = [[ord(c) - ord('a') for c in row] for row in dat]
sx, sy = 0, 20
ex, ey = 40, 20
cx, cy = len(dat[0]), len(dat)
dat[sy][sx] = 0
dat[ey][ex] = 25

distmap = [[None for x in range(cx)] for y in range(cy)]
distmap[ey][ex] = 0
unseen = cx * cy - 1

while unseen > 0:
	try:
		tx, ty, n = min((
				(x, y, distmap[y + dy][x + dx] + 1)
				for y in range(cy)
				for x in range(cx)
				if distmap[y][x] is None
				for dy, dx in [(-1,0),(1,0),(0,-1),(0,1)]
				if 0 <= x + dx < cx and 0 <= y + dy < cy
				and distmap[y + dy][x + dx] is not None
				and dat[y][x] >= dat[y + dy][x + dx] - 1
			), key=lambda x: x[2]
		)
	except ValueError:
		break
	else:
		distmap[ty][tx] = n
		unseen -= 1

print(distmap[sy][sx])
print(min(distmap[y][x] for y in range(cy) for x in range(cx) if dat[y][x] == 0 and distmap[y][x] is not None))
