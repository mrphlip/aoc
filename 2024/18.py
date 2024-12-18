#!/usr/bin/python
from aocimports import *

dat = [
	(27,6),
	(24,29),
	[...]
]

CX = CY = 71
N = 1024

#dat = [(5,4),(4,2),(4,5),(3,0),(2,1),(6,3),(2,4),(1,5),(0,6),(3,3),(2,6),(5,1),(1,2),(5,5),(2,5),(6,5),(1,4),(0,4),(6,4),(1,1),(6,1),(1,0),(0,5),(1,6),(2,0),]
#CX = CY = 7
#N = 12

def solve(nblocks):
	blocks = set(dat[:nblocks])
	todo = {(0,0)}
	solved = set()
	best = {(0,0):0}
	while todo and (CX-1,CY-1) not in solved:
		x, y = min(todo, key=best.__getitem__)
		todo.remove((x, y))
		if (x, y) in solved:
			continue
		solved.add((x, y))
		d = best[x, y]
		for nx, ny in [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]:
			if 0 <= nx < CX and 0 <= ny < CY and (nx, ny) not in solved and (nx, ny) not in blocks and ((nx, ny) not in best or best[nx,ny] > d + 1):
				best[nx, ny] = d + 1
				todo.add((nx, ny))
	if (CX-1,CY-1) in solved:
		return best[CX-1,CY-1]
	else:
		return None

print(solve(N))

nmin = 0
nmax = len(dat) + 1
while nmin < nmax - 1:
	nmid = (nmin + nmax) // 2
	if solve(nmid) is None:
		nmax = nmid
	else:
		nmin = nmid
print(dat[nmin])
