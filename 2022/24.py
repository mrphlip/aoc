#!/usr/bin/python3
dat = """\
#.########################################################################################################################
#><..<.<<^v><<^>>>^^^<v<>..^<<^<<^^<.<vv><vv<v^>.<>^<v>.<>^v>>>v^><.>.<<>>>^v<>vv<vv>^v^vvv>vv<<v><^<>>^^<><^^><.^v>.^>.<#
#>>v<vv<<<<<v<^><<>v.^>^<<<.<^.<<^<.^v^<^^>v<v>v^<v^^v<><>^>>><<^v^^<<v>v^><...^<<<><>>^v>^>><^<<.^v^>v>>>^><vv>^v^.^v><<#
[...snip...]
########################################################################################################################.#"""
#dat = "#.######\n#>>.<^<#\n#.<..<<#\n#>v.><>#\n#<^v^^>#\n######.#"

dat = dat.split("\n")
dat = [i[1:-1] for i in dat[1:-1]]
cy = len(dat)
cx = len(dat[0])

dirs = {c:i for i,c in enumerate(">v<^")}
blizzards = [(x, y, dirs[c]) for y,row in enumerate(dat) for x,c in enumerate(row) if c != '.']
orig_blizzards = blizzards[:]
blizzard_set = None
blizzard_set2 = None

def step_blizzard(x, y, d):
	if d == 0:
		x = (x + 1) % cx
	elif d == 1:
		y = (y + 1) % cy
	elif d == 2:
		x = (x - 1) % cx
	elif d == 3:
		y = (y - 1) % cy
	return (x, y, d)
def step_blizzards():
	global blizzards, blizzard_set, blizzard_set2
	blizzards = [step_blizzard(x, y, d) for x, y, d in blizzards]
	blizzard_set = set((x, y) for x, y, d in blizzards)
	blizzard_set2 = set((x, y, n) for x, y, d in blizzards for n in [0,1,2])

possible_places = {(0,-1)}
i = 0
while True:
	step_blizzards()
	i += 1
	if i % 100 == 0:
		print("  ", i)
	possible_places = possible_places | set((x+dx,y+dy) for x, y in possible_places for dx, dy in [(-1,0),(1,0),(0,-1),(0,1)] if 0 <= x + dx < cx and 0 <= y + dy < cy)
	possible_places = possible_places - blizzard_set
	if (cx-1, cy-1) in possible_places:
		i += 1
		break
print(i)

blizzards = orig_blizzards

possible_places = {(0,-1,0)}
i = 0
while True:
	step_blizzards()
	i += 1
	if i % 100 == 0:
		print("  ", i)
	prev_possible_places = possible_places
	possible_places = possible_places | set((x+dx,y+dy,n) for x, y,n in possible_places for dx, dy in [(-1,0),(1,0),(0,-1),(0,1)] if 0 <= x + dx < cx and 0 <= y + dy < cy)
	possible_places = possible_places - blizzard_set2
	if (cx-1,cy-1,0) in prev_possible_places:
		possible_places.add((cx-1,cy,1))
	if (0,0,1) in prev_possible_places:
		possible_places.add((0,-1,2))
	if (cx-1, cy-1, 2) in possible_places:
		i += 1
		break
print(i)
