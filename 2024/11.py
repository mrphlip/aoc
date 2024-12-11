#!/usr/bin/python
from collections import defaultdict, Counter
from fractions import Fraction
from copy import deepcopy
from pprint import pprint
import re, math
import itertools

dat = (7568, 155731, ...)

#dat = (125, 17)

#def step(line):
#	def gen():
#		for i in line:
#			if i == 0:
#				yield 1
#			else:
#				s = str(i)
#				if len(s) % 2 == 0:
#					yield int(s[:len(s)//2])
#					yield int(s[len(s)//2:])
#				else:
#					yield i*2024
#	return tuple(gen())
#
#a = dat
#for i in range(25):
#	a = step(a)
#print(len(a))

CACHE = {}

def score(n, levels):
	if (n, levels) not in CACHE:
		CACHE[n, levels] = calcscore(n, levels)
	return CACHE[n, levels]

def calcscore(n, levels):
	if levels <= 0:
		return 1
	if n == 0:
		return score(1, levels-1)
	s = str(n)
	if len(s) % 2 == 0:
		return score(int(s[:len(s)//2]), levels-1) + score(int(s[len(s)//2:]), levels-1)
	else:
		return score(n*2024, levels-1)

print(sum(score(i, 25) for i in dat))
print(sum(score(i, 75) for i in dat))
