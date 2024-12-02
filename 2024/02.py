#!/usr/bin/python
import re

dat = [
	(1,3,5,6,8,9,12,9),
	(66,67,70,72,73,74,75,75),
	(18,20,22,25,28,31,35),
	# ...
]

def safe(s):
	s = list(s)
	s_ = list(sorted(s))
	if s == s_ or s == s_[::-1]:
		if all(i != j and j-3 <= i <= j+3 for i,j in zip(s,s[1:])):
			return True
	return False
print(sum(1 for i in dat if safe(i)))

def safe2(s):
	return safe(s) or any(safe(s[:i]+s[i+1:]) for i in range(len(s)))
print(sum(1 for i in dat if safe2(i)))
