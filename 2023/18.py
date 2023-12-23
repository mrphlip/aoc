#!/usr/bin/python
dat = [
	("L", 7, 0x50a670),
	("U", 3, 0x1fa0d3),
	("R", 7, 0x50a672),
	("U", 3, 0x541033),
	[...snip...]
]

#dat = [("R", 6, 0x70c710),("D", 5, 0x0dc571),("L", 2, 0x5713f0),("D", 2, 0xd2c081),("R", 2, 0x59c680),("D", 2, 0x411b91),("L", 5, 0x8ceee2),("U", 2, 0xcaa173),("L", 1, 0x1b58a2),("U", 2, 0xcaa171),("R", 2, 0x7807d2),("U", 3, 0xa77fa3),("L", 2, 0x015232),("U", 2, 0x7a21e3),]

dirs = {
	'U': (0,-1),
	'D': (0,1),
	'L': (-1,0),
	'R': (1,0),
}

border = {}
isup = set()
x = y = 0
for d, l, c in dat:
	border[x,y] = c
	dx, dy = dirs[d]
	for i in range(l):
		if dy < 0:
			isup.add((x,y))
		x += dx
		y += dy
		border[x,y] = c
		if dy > 0:
			isup.add((x,y))
minx = min(x for x,y in border)
maxx = max(x for x,y in border) + 1
miny = min(y for x,y in border)
maxy = max(y for x,y in border) + 1
grid = []
count = 0
for y in range(miny, maxy):
	row = []
	i = False
	for x in range(minx, maxx):
		if (x,y) in border:
			row.append(True)
			count += 1
			if (x,y) in isup:
				i = not i
		else:
			row.append(i)
			count += i
	grid.append(row)
print(count)

path = []
ys = {0}
x = y = 0
for d, l, c in dat:
	prevx, prevy = x, y
	d = "RDLU"[c & 0xF]
	l = c >> 4
	dx, dy = dirs[d]
	x += dx * l
	y += dy * l
	path.append((prevx, prevy, x,y))
	ys.add(y)
ys = list(sorted(ys))
def score_row(verts, horizs):
	xs = verts + [i[0] for i in horizs] + [i[1] for i in horizs]
	xs = list(sorted(set(xs)))
	n = len(xs)
	i = False
	for x1, x2 in zip(xs[:-1], xs[1:]):
		if x2 == x1 + 1:
			continue
		x = x1 + 1
		if x1 in verts:
			i = not i
		if any(xa < x < xb for xa,xb in horizs) or i:
			n += x2-x1-1
	return n
# first, count up the boundary lines
count = 0
for y in ys:
	verts = [x for x,y1,_,y2 in path if y1 <= y < y2 or y2 <= y < y1]
	horizs = [(min(x1, x2),max(x1,x2)) for x1,y1,x2,_ in path if y1 == y]
	count += score_row(verts, horizs)
# now count up the inbetweens
for y1, y2 in zip(ys[:-1],ys[1:]):
	if y2 == y1 + 1:
		continue
	verts = [x for x,ya,_,yb in path if ya <= y1+1 < yb or yb <= y1+1 < ya]
	count += (y2 - y1 - 1) * score_row(verts, [])
print(count)
