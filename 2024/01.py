#!/usr/bin/python
import re

dat = [
	(15244,   50562),
	(81245,   49036),
	# ...
]

a = [i[0] for i in dat]
b = [i[1] for i in dat]
a.sort()
b.sort()
n = sum(abs(i-j) for i,j in zip(a,b))
print(n)

from collections import Counter
b = Counter(b)
n = sum(b.get(i, 0)*i for i in a)
print(n)
