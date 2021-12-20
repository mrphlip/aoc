dat1 = """#..##...[etc]"""
dat2 = """.##...##..####.##.##.#...######.#.#..#.#.#..#.#.#.##....##.#..#..#..####.#..########.##.###.#.##.###
[etc]"""

#dat1 = "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"; dat2 = "#..#.\n#....\n##..#\n..#..\n..###"

dat1 = [c == '#' for c in dat1]
dat2 = [[c == '#' for c in i] for i in dat2.split("\n")], False

def enh(state):
	grid, border = state
	cy = len(grid)
	cx = len(grid[0])
	def get(x,y):
		if 0 <= x < cx and 0 <= y < cy:
			return grid[y][x]
		else:
			return border
	res = []
	for y in range(-1,cy+1):
		row = []
		for x in range(-1,cx+1):
			a = int(''.join('1' if get(x+dx,y+dy) else '0' for dy in [-1,0,1] for dx in [-1,0,1]), 2)
			row.append(dat1[a])
		res.append(row)
	newborder = dat1[511] if border else dat1[0]
	return res, newborder

def prnt(state):
	grid = state[0]
	for row in grid:
		print(''.join("#" if cell else " " for cell in row))


g1 = enh(dat2)
g2 = enh(g1)

#prnt(dat2)
#print("===")
#prnt(g1)
#print("===")
#prnt(g2)

print(len([1 for row in g2[0] for cell in row if cell]))

g = g2
for i in range(48):
	g = enh(g)

print(len([1 for row in g[0] for cell in row if cell]))
