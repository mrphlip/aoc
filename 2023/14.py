#!/usr/bin/python
dat = """\
#..O...O...O....##O#O#O..O.#...O..O..#O.#O.#.O....##.OOOO....O.O.OO..O##..O..O....#O#....#..O.#.#.O.
OO.....O.O...##..#.#.......#...#.O.#.#..#...#...#....##O......#O#........#...O......O.#......#O.O...
[...snip...]""".split("\n")

#dat = "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#....".split("\n")

CY = len(dat)
CX = len(dat[0])

cubes = set()
balls = set()
for y in range(CY):
	for x in range(CX):
		if dat[y][x] == '#':
			cubes.add((x,y))
		elif dat[y][x] == 'O':
			balls.add((x,y))

def roll(balls, dx, dy):
	rolledballs = set()
	for x,y in sorted(balls, key=lambda i: (i[0]*dx+i[1]*dy), reverse=True):
		while 0 <= x+dx < CX and 0 <= y+dy < CY and (x+dx,y+dy) not in cubes and (x+dx,y+dy) not in rolledballs:
			x += dx
			y += dy
		rolledballs.add((x,y))
	return rolledballs

rolledballs = roll(balls, 0, -1)
score = sum(CY-y for x,y in rolledballs)
print(score)

rolledballs = balls
n = 0
seen = {}
hist = []
key = tuple(sorted(rolledballs))
while n < 1000000000 and key not in seen:
	seen[key] = n
	hist.append(rolledballs)
	rolledballs = roll(rolledballs, 0, -1)
	rolledballs = roll(rolledballs, -1, 0)
	rolledballs = roll(rolledballs, 0, 1)
	rolledballs = roll(rolledballs, 1, 0)
	key = tuple(sorted(rolledballs))
	n += 1
#print(n, seen[key])
step = n - seen[key]
ofs = (1000000000 - seen[key]) % step
res = hist[seen[key] + ofs]
score = sum(CY-y for x,y in res)
print(score)
