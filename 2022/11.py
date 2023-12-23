#!/usr/bin/python3
from copy import deepcopy
from math import lcm
from functools import reduce
dat = [
	([56, 52, 58, 96, 70, 75, 72],
	'*', 17,
	11,
	2,
	3,),
	[...snip...]
]
#dat = [([79, 98], '*', 19, 23, 2, 3,), ([54, 65, 75, 74], '+', 6, 19, 2, 0,), ([79, 60, 97], '^', 2, 13, 1, 3,), ([74], '+', 3, 17, 0, 1,)]
modulo = reduce(lcm, [i[3] for i in dat])
origdat = deepcopy(dat)

count = [0] * len(dat)

def turn(part1):
	for n, m in enumerate(dat):
		for i in m[0]:
			if m[1] == '+':
				i += m[2]
			elif m[1] == '*':
				i *= m[2]
			else:
				i *= i
			if part1:
				i //= 3
			else:
				i %= modulo
			target = m[4] if i % m[3] == 0 else m[5]
			dat[target][0].append(i)
			count[n] += 1
		m[0][:] = []
for i in range(20):
	turn(True)
count.sort()
print(count[-1] * count[-2])

dat = origdat
count = [0] * len(dat)
for i in range(10000):
	turn(False)
count.sort()
print(count[-1] * count[-2])
