#!/usr/bin/python
dat = """\
F--F7F7|7FJ..JF-7F|77FL-L7.L.L7LFFJ-77|7.L777LF7L|FJJ-7-F|J7-7FF-7..FFF7JF--L77FL--77-7-77-|7FJ.LL.|7L7-|-FJJ.-J.F-77|F---|7.F--|--.F-7-7.F7
FJ-|J|JJLF7F77|||FJ7J-|J|LF7.L||LJ|L77LL-JL--F--.LLJ|-..7|FJ..FJ7F.J.|7L-L7.|--F.7J||L|J.JJLFJF77|-LJ-|-J-FJ-F|J|-7|-L|JJ.L7.|-F77-FFJLLF.L-
[...snip...]""".split("\n")

CY = len(dat)
CX = len(dat[0])

for y in range(CY):
	for x in range(CX):
		if dat[y][x] == "S":
			dat[y] = dat[y][:x] + "F" + dat[y][x+1:]
			sx, sy = x, y
			break

up = (0,-1)
down = (0,1)
left = (-1,0)
right = (1,0)
neighbours = {
	'|': (up, down),
	'-': (left, right),
	'L': (up, right),
	'J': (up, left),
	'7': (down, left),
	'F': (down, right),
	'.': (),
}

dists = {}
todo = {(sx,sy):0}
while todo:
	x, y = min(todo.keys(), key=todo.__getitem__)
	d = todo.pop((x, y))
	dists[x,y] = d
	for dx, dy in neighbours[dat[y][x]]:
		if (x+dx, y+dy) not in dists:
			if (x+dx, y+dy) not in todo or todo[(x+dx, y+dy)] > d+1:
				todo[(x+dx, y+dy)] = d + 1
# print(max(dists.values())) ???
print(len(dists) // 2)


n = 0
for y in range(CY):
	for x in range(CX):
		if (x, y) in dists:
			#print('@', end='')
			continue
		inside = False
		for x2 in range(x, CY):
			if (x2, y) in dists and up in neighbours[dat[y][x2]]:
				inside = not inside
		if inside:
			n += 1
		#print('.' if inside else ' ', end='')
	#print()
print(n)
