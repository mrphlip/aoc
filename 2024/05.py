#!/usr/bin/python
from collections import defaultdict, Counter
from fractions import Fraction
from copy import deepcopy
from pprint import pprint
import re, math

dat = [
	(69,26),
	(93,46),
	(93,43),
	(46,53),
	# ...
]

dat2 = [
	(57,47,82,32,18),
	(74,56,86,81,84,44,53,92,12,36,15,66,95,26,71),
	(26,68,47,42,73,41,52,44,78,64,24,76,29,82,38),
	(98,78,59,22,91),
	# ...
]

pairs = set(dat)

n = 0
for row in dat2:
	if all((row[j], row[i]) not in pairs for i in range(len(row)-1) for j in range(i+1,len(row))):
		n += row[len(row)//2]
print(n)

from functools import cmp_to_key
def cmp(a, b):
	if (a,b) in pairs:
		return 1
	elif (b,a) in pairs:
		return -1
	else:
		1/0
key = cmp_to_key(cmp)
n = 0
for row in dat2:
	if all((row[j], row[i]) not in pairs for i in range(len(row)-1) for j in range(i+1,len(row))):
		continue
	row = list(row)
	row.sort(key=key)
	n += row[len(row)//2]
print(n)
