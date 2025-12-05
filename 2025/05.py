#!/usr/bin/python
from aocimports import *

dat = [
	(169486974574545,170251643963353),
	(350457710225863,350888576149828),
	[...]
]
dat2 = [
	166774327825644,
	91047458369966,
	[...]
]

#dat = [(3,5),(10,14),(16,20),(12,18)]; dat2 = [1,5,8,11,17,32]

dat = [(i,j+1) for i,j in dat]

dat.sort()

flattened = []
for i,j in dat:
	if not flattened or i >= flattened[-1][1]:
		flattened.append((i, j))
	else:
		flattened[-1] = (flattened[-1][0], max(flattened[-1][1], j))

def findrange(x):
	a = 0
	b = len(flattened)
	while a < b:
		c = (a + b) // 2
		if flattened[c][0] <= x < flattened[c][1]:
			return flattened[c]
		elif x < flattened[c][0]:
			b = c
		else:
			a = c + 1
	return None

count = 0
for i in dat2:
	if findrange(i) is not None:
		count += 1
print(count)

print(sum(j-i for i,j in flattened))
