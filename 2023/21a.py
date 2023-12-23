#!/usr/bin/python
dat = """\
.......
.#.##.#
.#.##.#
.#.##.#
.#.#..#
.#.#..#
.#.##.#""".split("\n")

dat = """.......\n.#..#.#\n.#.#..#\n.#..#.#\n.#.#..#\n.#..#.#\n.#.#..#""".split("\n")

#dat = ".....\n.....\n.....\n.....\n.....".split("\n")

dat = """\
...................................................................................................................................
...##......#....#...#.....#.#....#...#...#...#......#........................##......#..#..#.#........#....................##......
...#.......#..........#..#....#................#...##.....##...................#..........###......#.....#.........#....#.#........
[...snip...]""".split("\n")

#dat = """.......\n.#..#.#\n.#.#..#\n.#..#.#\n.#.#..#\n.#..#.#\n.#.#..#""".split("\n")

#startx = starty = 0
startx = starty = 65
#CX = CY = 7
CX = CY = 131
#N = 29
#N = 300
N = 551

dat = dat[starty:] + dat[:starty]
dat = [i[startx:] + i[:startx] for i in dat]
startx = starty = 0

dists = {}
todo = [(startx,starty,0)]
while todo:
	x, y, d = todo.pop(0)
	if d > N:
		break
	if (x,y) in dists:
		continue
	if dat[y%CY][x%CX] == '#':
		continue
	dists[x,y] = d
	for dx, dy in [(0,-1),(0,1),(1,0),(-1,0)]:
		todo.append((x+dx,y+dy,d+1))
print(len([1 for d in dists.values() if d <= N and (d-N)%2 == 0]))

minx = min(x for x,y in dists.keys())
maxx = max(x for x,y in dists.keys())+1
miny = min(y for x,y in dists.keys())
maxy = max(y for x,y in dists.keys())+1

totals = {}

#for y in range(miny,maxy):
#	for x in range(minx, maxx):
#		col = ((x // CX) * 3 + (y // CY)) % 7
#		print("\x1b[%dm" % (31+col), end='')
#		if dat[y%CY][x%CX] == '#':
#			print('#',end='')
#		elif dists.get((x,y),-1)%2 == 0:
#			print('O',end='')
#		else:
#			print('.',end='')
#	print()
#print("\x1b[0m", end='')

for y in range(miny,maxy):
	for x in range(minx, maxx):
		if dists.get((x,y),N+1)%2 == N%2:
			bx = x // CX
			by = y // CY
			if (bx,by) in totals:
				totals[bx,by] += 1
			else:
				totals[bx,by] = 1
#for y in range(miny//CY,(maxy-1)//CY+1):
#	for x in range(minx//CX,(maxx-1)//CX+1):
#		print("%03s" % totals.get((x,y),'-'), end=' ')
#	print()
from collections import Counter
print(Counter(totals.values()))
print(sum(totals.values()))

#R = 2
#import colorsys
#with open("21.ppm", "w") as fp:
#	print("P3", file=fp)
#	print(f"{maxx-minx} {maxy-miny}", file=fp)
#	print("255", file=fp)
#	for y in range(miny,maxy):
#		for x in range(minx, maxx):
#			bx = x // CX
#			by = y // CY
#			dist = abs(bx) + abs(by)
#			if dist <= R:
#				col = "255 0 0"
#			elif dist <= R + 3 or (bx < 0 and by < 0 and dist <= R + 4):
#				if bx < 0 and by < 0:
#					n = dist - R - 1
#				elif by < 0:
#					n = dist - R + 3
#				elif bx < 0:
#					n = dist - R + 6
#				else:
#					n = dist - R + 9
#				n = (n * 4) % 13
#				r,g,b = colorsys.hsv_to_rgb((n+1)/14.0, 1,1)
#				r = int(r*255)
#				g = int(g*255)
#				b = int(b*255)
#				col = f"{r} {g} {b}"
#			else:
#				col = "255 0 255"
#			if dat[y%CY][x%CX] == '#':
#				print('0 0 0',end=' ',file=fp)
#			elif (x,y) in dists:
#				print(col,end=' ',file=fp)
#			else:
#				print('255 255 255',end=' ',file=fp)
#		print(file=fp)
