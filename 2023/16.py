#!/usr/bin/python
dat = r"""\....................\...|...................\/........|.....-......|....|................................/...
...../.../........|.........../......-........-...|.....|............\............./..../.\.\.................
|................./..../...............................................-....................\......../........
[...snip...]""".split("\n")

#dat = ".|...\\....\n|.-.\\.....\n.....|-...\n........|.\n..........\n.........\\\n..../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|....".split("\n")

CY = len(dat)
CX = len(dat[0])

def trace(x, y, dx, dy):
	seen = set()
	points = set()
	beams = [(x, y, dx, dy)]
	while beams:
		x, y, dx, dy = beams.pop(0)
		if (x, y, dx, dy) in seen:
			continue
		if not (0 <= x < CX and 0 <= y < CY):
			continue
		seen.add((x,y,dx,dy))
		points.add((x,y))
		c = dat[y][x]
		if c == '.':
			beams.append((x+dx, y+dy, dx, dy))
		elif c == '/':
			dx, dy = -dy, -dx
			beams.append((x+dx, y+dy, dx, dy))
		elif c == '\\':
			dx, dy = dy, dx
			beams.append((x+dx, y+dy, dx, dy))
		elif c == '|':
			if dx:
				beams.append((x, y+1, 0, 1))
				beams.append((x, y-1, 0, -1))
			else:
				beams.append((x+dx, y+dy, dx, dy))
		elif c == '-':
			if dy:
				beams.append((x+1, y, 1, 0))
				beams.append((x-1, y, -1, 0))
			else:
				beams.append((x+dx, y+dy, dx, dy))
		else:
			raise ValueError(f"Unknown char {c}")
	return len(points)
print(trace(0,0,1,0))

def gen():
	for y in range(CY):
		yield trace(0, y, 1, 0)
		yield trace(CX-1, y, -1, 0)
	for x in range(CX):
		yield trace(x, 0, 0, 1)
		yield trace(x, CY-1, 0, -1)
print(max(gen()))
