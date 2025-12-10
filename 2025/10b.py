#!/usr/bin/python -u
from aocimports import *

dat = [
	("##.....#.", (0,1,5,8), (1,6,7), (3,6,8), (1,3,6,7), (0,1,2,6,7), (1,2,3,5,7), (0,1,3,4,5,6,7), (1,2,4,5,7,8), (0,2,5,7,8), (1,2,3,5,7,8), (53,78,43,44,33,73,46,81,60)),
	("..#..##", (2,3,5,6), (0,1,2,4,5,6), (0,1,5,6), (0,1,2,5), (2,4,5,6), (2,5,6), (4,5), (2,3,5), (29,29,222,175,49,256,223)),
	[...]
]

#dat = [(".##.", (3,), (1,3), (2,), (2,3), (0,2), (0,1), (3,5,4,7)),("...#.", (0,2,3,4), (2,3), (0,4), (0,1,2), (1,2,3,4), (7,5,12,7,2)),(".###.#", (0,1,2,3,4), (0,3,4), (0,1,2,4,5), (1,2), (10,11,11,5,10,5)),]

def solve2(row):
	orig_target = target = row[-1]
	buttons = row[1:-1]

	T = len(target)
	B = len(buttons)

	matrix = [[Fraction(0)] * (T+B) for i in range(B)]
	for i, b in enumerate(buttons):
		for j in b:
			matrix[i][j] = Fraction(1)
		matrix[i][i+T] = Fraction(1)
	#print(matrix)
	# gauss jordan: triangulate
	a = 0
	for i in range(T):
		for j in range(a, B):
			if matrix[j][i]:
				break
		else:
			continue
		if j != a:
			matrix[a], matrix[j] = matrix[j], matrix[a]
		for j in range(a+1, B):
			if matrix[j][i]:
				k = matrix[j][i] / matrix[a][i]
				matrix[j] = [b-a*k for a,b in zip(matrix[a],matrix[j])]
		a += 1
	#print(matrix)
	# backsolve
	for j in range(B-1,-1,-1):
		first = [ix for ix, x in enumerate(matrix[j]) if x]
		if not first or first[0] >= T:
			continue
		first = first[0]
		for i in range(j-1,-1,-1):
			if matrix[i][first]:
				k = matrix[i][first] / matrix[j][first]
				matrix[i] = [b-a*k for a,b in zip(matrix[j],matrix[i])]
	#print(matrix)
	soln = [0]*B
	for row in matrix:
		first = [ix for ix, x in enumerate(row) if x]
		if not first or first[0] >= T:
			continue
		first = first[0]
		#print("  ",target)
		#print("  ",soln)
		if target[first]:
			k = target[first] / row[first]
			target = [a-b*k for a,b in zip(target, row)]
			soln = [a+b*k for a,b in zip(soln, row[T:])]
	#print(target)
	#print(soln)
	assert not any(target)

	noops = [row[T:] for row in matrix if not any(row[:T])]
	
	# so, soln + any linear combination of noops will work
	# but how do we limit it to just non-negative integers

	#print(soln, noops)
	N = len(noops)
	ranges = [[None, None, None] for i in range(N)]
	for i in range(B):
		covered = [ix for ix, row in enumerate(noops) if row[i]]
		if len(covered) == 1:
			cov = covered[0]
			if noops[cov][i] > 0:
				newrange = [-soln[i]/noops[cov][i], None, 1/noops[cov][i]]
			else:
				newrange = [None, -soln[i]/noops[cov][i], -1/noops[cov][i]]
			#print(cov, soln[i], noops[cov][i], newrange, ranges[cov])
			ranges[cov] = foldrange(ranges[cov], newrange)
			#print("  ", ranges[cov])
	#print(ranges)
	assert all(i[2] is not None for i in ranges)

	print("  ", max(orig_target), len([i for i in ranges if i[0] is None or i[1] is None]))

	def options(ix, s, limit):
		if ix >= N:
			if all(i >= 0 and i.denominator == 1 for i in s):
				yield s
			return
		rmin, rmax, rstep = ranges[ix]
		if rmin is None:
			rmin = rmax - rstep * limit
		if rmax is None:
			rmax = rmin + rstep * limit
		i = rmin
		while i <= rmax:
			yield from options(ix + 1, [a+i*b for a,b in zip(s, noops[ix])], limit)
			#print(i)
			i += rstep

	#print(list(options(0, soln, 0)))
	return min(sum(i) for i in options(0, soln, max(orig_target)))

def foldrange(a, b):
	amin, amax, astep = a
	bmin, bmax, bstep = b
	if astep is None:
		return b
	if bstep is None:
		return a

	d = lcm(*[i.denominator for i in [amin, amax, astep, bmin, bmax, bstep] if i is not None])
	amin = int(amin * d) if amin is not None else None
	amax = int(amax * d) if amax is not None else None
	astep = int(astep * d) if astep is not None else None
	bmin = int(bmin * d) if bmin is not None else None
	bmax = int(bmax * d) if bmax is not None else None
	bstep = int(bstep * d) if bstep is not None else None

	newstep = lcm(astep, bstep)
	newofs = crt(amin if amin is not None else amax, astep, bmin if bmin is not None else bmax, bstep)[0]
	if amin is not None and bmin is not None:
		newmin = max(amin, bmin)
	else:
		newmin = amin if amin is not None else bmin
	if amax is not None and bmax is not None:
		newmax = min(amax, bmax)
	else:
		newmax = amax if amax is not None else bmax

	if newmin is not None:
		x = (newmin - newofs) % newstep
		if x:
			newmin = newmin - x + newstep
		newmin = Fraction(newmin, d)
	if newmax is not None:
		x = (newmax - newofs) % newstep
		if x:
			newmax = newmax - x
		newmax = Fraction(newmax, d)
	newstep = Fraction(newstep, d)

	return [newmin, newmax, newstep]

n = 0
for ix, row in enumerate(dat[15:16]):
	res = solve2(row)
	n += res
	print(ix, res, n)
print(n)
