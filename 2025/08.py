#!/usr/bin/python
from aocimports import *

dat = [
	(57869,43825,4618),
	(33342,30491,49401),
	[...]
]
N=1000

#dat = (162,817,812),(57,618,57),(906,360,560),(592,479,940),(352,342,300),(466,668,158),(542,29,236),(431,825,988),(739,650,466),(52,470,668),(216,146,977),(819,987,18),(117,168,530),(805,96,715),(346,949,466),(970,615,88),(941,993,340),(862,61,35),(984,92,344),(425,690,689); N=10

dists = [
	(i, j, (x2-x1)**2+(y2-y1)**2+(z2-z1)**2)
	for i, (x1, y1, z1) in enumerate(dat)
	for j, (x2, y2, z2) in enumerate(dat)
	if i < j
]
dists.sort(key=lambda x: x[2])

connected = {i: {i} for i in range(len(dat))}
joins = []
for i, j, dist in dists[:N]:
	if i in connected[j]:
		continue
	joins.append((i, j))
	conn = connected[i] | connected[j]
	for x in conn:
		connected[x] = conn
sizes = [len(v) for k,v in connected.items() if k == min(v)]
sizes.sort()
print(prod(sizes[-3:]))

for i, j, dist in dists[N:]:
	if i in connected[j]:
		continue
	joins.append((i, j))
	conn = connected[i] | connected[j]
	for x in conn:
		connected[x] = conn
	if len(joins) >= len(dat) - 1:
		break
i, j = joins[-1]
print(dat[i][0] * dat[j][0])
