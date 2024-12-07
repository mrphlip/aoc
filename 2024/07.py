#!/usr/bin/python
from collections import defaultdict, Counter
from fractions import Fraction
from copy import deepcopy
from pprint import pprint
import re, math
import itertools

dat = [
	(2382106471,(2,8,175,1,17,3,5,9,4,51,5)),
	(864708004,(278,22,259,2,12,3)),
	[...]
]

#dat = [(190,(10,19)),(3267,(81,40,27)),(83,(17,5)),(156,(15,6)),(7290,(6,8,6,15)),(161011,(16,10,13)),(192,(17,8,14)),(21037,(9,7,18,13)),(292,(11,6,16,20))]

def solvable(vals, res, ops="+*"):
	for i in itertools.product(ops, repeat=len(vals)-1):
		if calc(vals, i) == res:
			return True
	return False

def calc(vals, ops):
	i = vals[0]
	for op, v in zip(ops, vals[1:]):
		if op == "+":
			i += v
		elif op == "*":
			i *= v
		elif op == "|":
			i = int(f"{i}{v}")
		else:
			1/0
	return i

n = 0
for res, vals in dat:
	if solvable(vals, res):
		n += res
print(n)

n = 0
for res, vals in dat:
	if solvable(vals, res, "+*|"):
		n += res
print(n)
