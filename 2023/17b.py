#!/usr/bin/python
dat = """\
314412134515235343233533333536633455653353226524463545367577644655564375735567774443767545555476326253354625332434443364224465621314212153555
541151354452244522426632553456444465355436252633745744545656765664467373344353475666347765767443443552326263264235346622346265464551153535535
[...snip...]"""

#dat = "2413432311323\n3215453535623\n3255245654254\n3446585845452\n4546657867536\n1438598798454\n4457876987766\n3637877979653\n4654967986887\n4564679986453\n1224686865563\n2546548887735\n4322674655533"

dat = [[int(j) for j in i] for i in dat.split("\n")]

CY = len(dat)
CX = len(dat[0])

def solve(minl, maxl):
	costs = {(0,0,0,0):0}
	solved = set()
	todo = {(0,0,0,0)}
	while todo:
		x, y, prevdx, prevdy = min(todo, key=costs.__getitem__)
		todo.remove((x,y,prevdx, prevdy))
		solved.add((x,y,prevdx, prevdy))
		for dx, dy in [[-1,0],[1,0],[0,-1],[0,1]]:
			if dx == prevdx and dy == prevdy:
				continue
			if dx == -prevdx and dy == -prevdy:
				continue
			for l in range(minl, maxl):
				dx2 = dx * l
				dy2 = dy * l
				if not (0 <= x+dx2 < CX and 0 <= y+dy2 < CY):
					continue
				if (x+dx2, y+dy2, dx, dy) in solved:
					continue
				newcost = costs[x,y,prevdx,prevdy]
				for i in range(1, l+1):
					newcost += dat[y+dy*i][x+dx*i]
				if (x+dx2, y+dy2, dx, dy) not in costs or costs[(x+dx2, y+dy2, dx, dy)] > newcost:
					costs[(x+dx2, y+dy2, dx, dy)] = newcost
				todo.add((x+dx2, y+dy2, dx, dy))
	return min(v for (x,y,_,_),v in costs.items() if x == CX-1 and y == CY-1)
#print(solve(1,4))
print(solve(4,11))
