#!/usr/bin/python
from collections import defaultdict, Counter
from fractions import Fraction
from copy import deepcopy
from pprint import pprint
import re, math
from itertools import count, cycle, repeat, chain, groupby, product, permutations, combinations
from functools import cache, cmp_to_key, partial, reduce

dat = [
	(+63, +14, +12, +37, 5921, 10432),
	[...]
]

#dat = [(+94, +34, +22, +67, 8400, 5400), (+26, +66, +67, +21, 12748, 12176), (+17, +86, +84, +37, 7870, 6450), (+69, +23, +27, +71, 18641, 10279)]

def solve(a,b,c,d,x,y):
	det = a*d-b*c
	if det == 0:
		raise ValueError("det == 0")
	det = Fraction(1,det)
	a, b, c, d = d*det, -b*det, -c*det, a*det
	ra, rb = a*x + c*y, b*x + d*y
	if ra == int(ra) and rb == int(rb):
		return int(ra), int(rb)
	else:
		return None, None

n = 0
for line in dat:
	a, b = solve(*line)
	if a is not None:
		n += a*3+b
print(n)

n = 0
for line in dat:
	a, b = solve(*line[:4], line[4] + 10000000000000, line[5] + 10000000000000)
	if a is not None:
		n += a*3+b
print(n)
