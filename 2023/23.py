#!/usr/bin/python
dat = """\
#.###########################################################################################################################################
#.#...#...###.....#...###...#...#.......#.....#.............#.........###...............#.......#.....#.....#.....#.......#...#.....###.....#
#.#.#.#.#.###.###.#.#.###.#.#.#.#.#####.#.###.#.###########.#.#######.###.#############.#.#####.#.###.#.###.#.###.#.#####.#.#.#.###.###.###.#
[...snip...]""".split("\n")

#dat = "#.#####################\n#.......#########...###\n#######.#########.#.###\n###.....#.>.>.###.#.###\n###v#####.#v#.###.#.###\n###.>...#.#.#.....#...#\n###v###.#.#.#########.#\n###...#.#.#.......#...#\n#####.#.#.#######.#.###\n#.....#.#.#.......#...#\n#.#####.#.#.#########v#\n#.#...#...#...###...>.#\n#.#.#v#######v###.###v#\n#...#.>.#...>.>.#.###.#\n#####v#.#.###v#.#.###.#\n#.....#...#...#.#.#...#\n#.#########.###.#.#.###\n#...###...#...#...#.###\n###.###.#.###v#####v###\n#...#...#.#.>.>.#.>.###\n#.###.###.#.###.#.#v###\n#.....###...###...#...#\n#####################.#".split("\n")

CY = len(dat)
CX = len(dat[0])

dirs = {
	'>': (1,0),
	'<': (-1,0),
	'v': (0,1),
	'^': (0,-1),
}
slopes = {}
for y, row in enumerate(dat):
	for x, c in enumerate(row):
		if c in '<>v^':
			slopes[x,y] = dirs[c]

regions = []
ix = 0
toregion = {}
todo = [(1,0)] + [(x+dx,y+dy) for (x,y),(dx,dy) in slopes.items()]
for sx, sy in todo:
	if (sx, sy) in toregion:
		continue
	region = set()
	subtodo = [(sx, sy)]
	while subtodo:
		x, y = subtodo.pop(0)
		if not (0 <= x < CX and 0 <= y < CY) or dat[y][x] != '.':
			continue
		if (x, y) in region:
			continue
		region.add((x,y))
		toregion[x,y] = ix
		for dx, dy in dirs.values():
			subtodo.append((x+dx,y+dy))
	regions.append(region)
	ix += 1

bridges = []
for (x,y),(dx,dy) in slopes.items():
	for dx2, dy2 in dirs.values():
		if (dx2 != dx or dy2 != dy) and 0 <= x+dx2 < CX and 0 <= y+dy2 < CY and dat[y+dy2][x+dx2] == '.':
			bridges.append((x+dx2,y+dy2,x+dx,y+dy, toregion[x+dx2,y+dy2], toregion[x+dx,y+dy]))
regionbridgein = [[] for i in range(len(regions))]
regionbridgeout = [[] for i in range(len(regions))]
for x1,y1,x2,y2,r1,r2 in bridges:
	regionbridgeout[r1].append((x1,y1,x2,y2))
	regionbridgein[r2].append((x2,y2,x1,y1))

regionbridgein[toregion[1,0]].append((1,0,1,0))
regionbridgeout[toregion[CX-2,CY-1]].append((CX-2,CY-1,CX-2,CY-1))

longestroutes = {}

def makelongestroute(region, x1, y1, x2, y2):
	def consider(rest, x, y):
		if (x,y) not in rest:
			return
		if x == x2 and y == y2:
			yield len(region) - len(rest) + 1
			return
		rest = rest.copy()
		rest.remove((x,y))
		for dx, dy in dirs.values():
			yield from consider(rest, x+dx, y+dy)
	return max(consider(region, x1, y1))

for rix in range(len(regions)):
	for x1,y1,_,_ in regionbridgein[rix] + regionbridgeout[rix]:
		for x2,y2,_,_ in regionbridgein[rix] + regionbridgeout[rix]:
			longestroutes[x1,y1,x2,y2] = makelongestroute(regions[rix], x1, y1, x2, y2)

def makelongestpath(backslopes=False):
	xtarget = CX - 2
	ytarget = CY - 1
	rtarget = toregion[xtarget, ytarget]
	def consider(rest, curregion, x, y, l):
		if curregion not in rest:
			return
		if curregion == rtarget:
			yield l + longestroutes[x, y, xtarget, ytarget]
			return
		rest = rest.copy()
		rest.remove(curregion)
		for x1, y1, x2, y2 in regionbridgeout[curregion]:
			nextregion = toregion[x2,y2]
			leglen = longestroutes[x,y,x1,y1]
			yield from consider(rest, nextregion, x2, y2, l+1+leglen)
		if backslopes:
			for x1, y1, x2, y2 in regionbridgein[curregion]:
				nextregion = toregion[x2,y2]
				leglen = longestroutes[x,y,x1,y1]
				yield from consider(rest, nextregion, x2, y2, l+1+leglen)
	return max(consider(set(range(len(regions))), 0, 1, 0, -1))
print(makelongestpath())
print(makelongestpath(True))
