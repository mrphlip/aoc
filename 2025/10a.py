#!/usr/bin/python
from aocimports import *

dat = [
	("##.....#.", (0,1,5,8), (1,6,7), (3,6,8), (1,3,6,7), (0,1,2,6,7), (1,2,3,5,7), (0,1,3,4,5,6,7), (1,2,4,5,7,8), (0,2,5,7,8), (1,2,3,5,7,8), (53,78,43,44,33,73,46,81,60)),
	("..#..##", (2,3,5,6), (0,1,2,4,5,6), (0,1,5,6), (0,1,2,5), (2,4,5,6), (2,5,6), (4,5), (2,3,5), (29,29,222,175,49,256,223)),
	[...]
]

#dat = [(".##.", (3,), (1,3), (2,), (2,3), (0,2), (0,1), (3,5,4,7)),("...#.", (0,2,3,4), (2,3), (0,4), (0,1,2), (1,2,3,4), (7,5,12,7,2)),(".###.#", (0,1,2,3,4), (0,3,4), (0,1,2,4,5), (1,2), (10,11,11,5,10,5)),]

def countbits(n):
	i = 0
	while n > 0:
		i += 1
		n &= n-1
	return i
def lowestbit(n):
	return n - (n & (n-1))

def solve1(row):
	target = row[0]
	buttons = row[1:-1]

	T = len(target)
	B = len(buttons)

	target = sum(1<<i for i,c in enumerate(target) if c == "#")
	#print(target)

	matrix = [[0, 0] for i in range(B)]
	for i, b in enumerate(buttons):
		for j in b:
			matrix[i][0] |= 1 << j
		matrix[i][1] = 1 << i
	#print(matrix)
	# gauss jordan: triangulate
	a = 0
	for i in range(T):
		for j in range(a, B):
			if matrix[j][0] & 1<<i:
				break
		else:
			continue
		if j != a:
			matrix[a], matrix[j] = matrix[j], matrix[a]
		for j in range(a+1, B):
			if matrix[j][0] & 1<<i:
				matrix[j] = [a^b for a,b in zip(matrix[a],matrix[j])]
		a += 1
	#print(matrix)
	# backsolve
	for j in range(B-1,-1,-1):
		for i in range(j-1,-1,-1):
			if matrix[i][0] & matrix[j][0]:
				matrix[i] = [a^b for a,b in zip(matrix[i],matrix[j])]
	#print(matrix)
	soln = 0
	for row in matrix:
		if lowestbit(row[0]) & target:
			target ^= row[0]
			soln ^= row[1]
	#print(target)
	assert not target
	def options(ix, s):
		if ix >= B:
			yield s
		elif matrix[ix][0]:
			yield from options(ix + 1, s)
		else:
			yield from options(ix + 1, s)
			yield from options(ix + 1, s ^ matrix[ix][1])
	#print(list(options(0, soln)))
	return min(countbits(i) for i in options(0, soln))

print(sum(solve1(row) for row in dat))
