#!/usr/bin/python
dat = """\
314412134515235343233533333536633455653353226524463545367577644655564375735567774443767545555476326253354625332434443364224465621314212153555
541151354452244522426632553456444465355436252633745744545656765664467373344353475666347765767443443552326263264235346622346265464551153535535
[...snip...]"""

#dat = "2413432311323\n3215453535623\n3255245654254\n3446585845452\n4546657867536\n1438598798454\n4457876987766\n3637877979653\n4654967986887\n4564679986453\n1224686865563\n2546548887735\n4322674655533"

dat = [[int(j) for j in i] for i in dat.split("\n")]

CY = len(dat)
CX = len(dat[0])

costs = {(0,0,0,0,0):0}
#paths = {(0,0,0,0,0):[]}
solved = set()
todo = {(0,0,0,0,0)}
while todo:
	x, y, dx, dy, l = min(todo, key=costs.__getitem__)
	todo.remove((x,y,dx,dy,l))
	solved.add((x,y,dx,dy,l))
	for dx2, dy2 in [[-1,0],[1,0],[0,-1],[0,1]]:
		if not (0 <= x+dx2 < CX and 0 <= y+dy2 < CY):
			continue
		if dx2 == -dx and dy2 == -dy:
			continue
		if dx2 == dx and dy2 == dy:
			newl = l + 1
			if newl > 3:
				continue
		else:
			newl = 1
		newcost = costs[x,y,dx,dy,l] + dat[y+dy2][x+dx2]
		if (x+dx2, y+dy2, dx2, dy2, newl) in solved:
			continue
		if (x+dx2, y+dy2, dx2, dy2, newl) not in costs or costs[(x+dx2, y+dy2, dx2, dy2, newl)] > newcost:
			costs[(x+dx2, y+dy2, dx2, dy2, newl)] = newcost
			#paths[(x+dx2, y+dy2, dx2, dy2, newl)] = paths[x, y, dx, dy, l] + [(dx2,dy2)]
			#paths[(x+dx2, y+dy2, dx2, dy2, newl)] = paths[x, y, dx, dy, l] + [(x+dx2,y+dy2)]
		todo.add((x+dx2, y+dy2, dx2, dy2, newl))
v = min(v for k,v in costs.items() if k[0] == CX-1 and k[1] == CY-1)
print(v)
